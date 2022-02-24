use std::cell::RefCell;
use std::collections::hash_map::{Entry, HashMap};
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::PathBuf;
use std::rc::Rc;

use smithay::backend::{
    allocator::dmabuf::Dmabuf,
    drm::{DevPath, DrmDevice, DrmError, DrmEvent, GbmBufferedSurface},
    egl::{EGLContext as EglContext, EGLDisplay as EglDisplay},
    libinput::{LibinputInputBackend, LibinputSessionInterface},
    renderer::{
        gles2::{Gles2Frame, Gles2Renderer, Gles2Texture},
        Bind, Frame, ImportDma, ImportEgl, Renderer, Transform,
    },
    session::{auto::AutoSession, Session, Signal as SessionSignal},
    udev::{primary_gpu, UdevBackend, UdevEvent},
    SwapBuffersError,
};
use smithay::reexports::*;
use smithay::utils::{
    signaling::{Linkable, SignalToken, Signaler},
    Logical, Point,
};
use smithay::wayland::{
    dmabuf::init_dmabuf_global,
    output::{Mode, PhysicalProperties},
    seat::CursorImageStatus,
};

use calloop::{
    timer::{Timer, TimerHandle},
    Dispatcher, EventLoop, LoopHandle, RegistrationToken,
};
use drm::control::{
    connector::{Interface, State as ConnectorState},
    crtc, Device,
};
use either::Either;
use gbm::Device as GbmDevice;
use image::{ImageBuffer, ImageFormat};
use input::Libinput;
use nix::{fcntl::OFlag, libc::dev_t};
use slog::{crit, debug, error, info, trace, warn, Logger};
use sugars::dur;
use wayland_server::{
    protocol::{wl_output::Subpixel, wl_surface::WlSurface},
    DispatchData, Display,
};
use xcursor::parser::Image;

use crate::render::render_layers_and_windows;
use crate::util::PseudoCell;
use crate::{
    cursor::Cursor,
    drawing::*,
    output_map::OutputMap,
    state::{Backend, State},
    window_map::WindowMap,
};

#[derive(Clone)]
pub struct SessionFd(RawFd);
impl AsRawFd for SessionFd {
    fn as_raw_fd(&self) -> RawFd {
        self.0
    }
}

#[derive(PartialEq)]
struct UdevOutputId {
    device_id: dev_t,
    crtc: crtc::Handle,
}

pub struct UdevData {
    pub session: AutoSession,
    gpu: Option<PathBuf>,
    backends: HashMap<dev_t, BackendData>,
    signaler: Signaler<SessionSignal>,
    pointer_image: Cursor,
    render_timer: TimerHandle<(u64, crtc::Handle)>,
}

impl Backend for UdevData {
    fn seat_name(&self) -> String {
        self.session.seat()
    }
}

pub type RenderSurface = GbmBufferedSurface<SessionFd>;

struct SurfaceData {
    surface: RenderSurface,
    #[cfg(feature = "debug")]
    fps: fps_ticker::Fps,
}

struct BackendData {
    _restart_token: SignalToken,
    surfaces: Rc<RefCell<HashMap<crtc::Handle, Rc<RefCell<SurfaceData>>>>>,
    pointer_images: HashMap<*const Image, Gles2Texture>,
    #[cfg(feature = "debug")]
    fps_texture: Gles2Texture,
    renderer: Rc<RefCell<Gles2Renderer>>,
    gbm: GbmDevice<SessionFd>,
    registration_token: RegistrationToken,
    event_dispatcher: Dispatcher<'static, DrmDevice<SessionFd>, State<UdevData>>,
    dev_id: u64,
}

pub fn run(log: Logger) {
    let mut event_loop = EventLoop::try_new().unwrap();
    let display = Rc::new(RefCell::new(Display::new()));

    let (session, notifier) = match AutoSession::new(log.clone()) {
        Some(v) => v,
        None => {
            crit!(log, "Could not initialize a session");
            return;
        }
    };
    let session_signal = notifier.signaler();

    let gpu = primary_gpu(&session.seat()).unwrap_or(None);

    let timer = Timer::new().unwrap();

    let data = UdevData {
        session,
        gpu,
        backends: HashMap::new(),
        signaler: session_signal.clone(),
        pointer_image: Cursor::load(&log),
        render_timer: timer.handle(),
    };

    let handle = event_loop.handle();

    let mut state = State::init(display.clone(), handle.clone(), data, log.clone(), true);

    {
        let cb = cb!((dev_id, crtc), state => state.render(dev_id, Some(crtc)));
        handle.insert_source(timer, cb).unwrap();
    }

    let udev_backend = match UdevBackend::new(state.seat_name.clone(), log.clone()) {
        Ok(v) => v,
        Err(e) => {
            crit!(log, "Failed to initialize udev backend"; "error" => e);
            return;
        }
    };

    let libinput_event_source = {
        let interface = LibinputSessionInterface::from(state.backend_data.session.clone());
        let mut context = Libinput::new_with_udev(interface);
        context.udev_assign_seat(&state.seat_name).unwrap();
        let mut backend = LibinputInputBackend::new(context, log.clone());
        backend.link(session_signal);

        let cb = cb!(event, state => state.process_input_event(event));
        handle.insert_source(backend, cb).unwrap()
    };

    let session_event_source = handle.insert_source(notifier, cb!()).unwrap();

    for (dev, path) in udev_backend.device_list() {
        state.device_added(dev, path.to_owned());
    }

    {
        let mut formats = Vec::new();
        for backend in state.backends() {
            formats.extend(backend.renderer.borrow().dmabuf_formats().cloned());
        }

        let handler = |buffer: &_, mut dispatch_data: DispatchData<'_>| {
            let state = dispatch_data.get::<State<UdevData>>().unwrap();
            for backend in state.backends() {
                let mut renderer = backend.renderer.borrow_mut();
                if renderer.import_dmabuf(buffer).is_ok() {
                    return true;
                }
            }
            false
        };
        init_dmabuf_global(&mut display.borrow_mut(), formats, handler, log.clone());
    }

    let udev_event_source = {
        let cb = cb!(event, state => match event {
            UdevEvent::Added { device_id, path } => state.device_added(device_id, path),
            UdevEvent::Changed { device_id } => state.device_changed(device_id),
            UdevEvent::Removed { device_id } => state.device_removed(device_id),
        });
        handle
            .insert_source(udev_backend, cb)
            .map_err(std::io::Error::from)
            .unwrap()
    };

    state.start_xwayland();

    while state.running.get() {
        // TODO: refresh rate stuff
        let dur = dur!(16 milli);
        let dispatch_result = event_loop.dispatch(dur, &mut state);

        if dispatch_result.is_err() {
            state.running.set(false);
        } else {
            display.borrow_mut().flush_clients(&mut state);
            state.window_map.borrow_mut().refresh();
            state.output_map.borrow_mut().refresh();
        }
    }

    state.window_map.borrow_mut().clear();

    handle.remove(session_event_source);
    handle.remove(libinput_event_source);
    handle.remove(udev_event_source);
}

fn scan_connectors(
    device: &mut DrmDevice<SessionFd>,
    gbm: &GbmDevice<SessionFd>,
    renderer: &mut Gles2Renderer,
    output_map: &mut OutputMap,
    signaler: &Signaler<SessionSignal>,
    log: &Logger,
) -> HashMap<crtc::Handle, Rc<RefCell<SurfaceData>>> {
    let res_handles = device.resource_handles().unwrap();

    let mut backends = HashMap::new();

    for conn in res_handles.connectors().iter().copied() {
        let conn_info = device.get_connector(conn).unwrap();
        if conn_info.state() != ConnectorState::Connected {
            continue;
        }
        info!(log, "Connected: {:?}", conn_info.interface());

        'enc: for enc in conn_info.encoders().iter().copied().flatten() {
            let enc_info = match device.get_encoder(enc) {
                Ok(v) => v,
                Err(_) => continue,
            };
            for crtc in res_handles.filter_crtcs(enc_info.possible_crtcs()) {
                let entry = match backends.entry(crtc) {
                    Entry::Vacant(v) => v,
                    _ => continue,
                };

                info!(
                    log,
                    "Trying to setup connector {:?}-{} with crtc {:?}",
                    conn_info.interface(),
                    conn_info.interface_id(),
                    crtc,
                );

                let mode = conn_info.modes()[0];

                let mut surface = match device.create_surface(crtc, mode, &[conn_info.handle()]) {
                    Ok(surface) => surface,
                    Err(e) => {
                        warn!(log, "Failed to create drm surface: {e}");
                        continue;
                    }
                };
                surface.link(signaler.clone());

                let renderer_formats = Bind::<Dmabuf>::supported_formats(renderer)
                    .expect("dmabuf renderer without formats");

                let surface = match GbmBufferedSurface::new(
                    surface,
                    gbm.clone(),
                    renderer_formats,
                    log.clone(),
                ) {
                    Ok(v) => v,
                    Err(e) => {
                        warn!(log, "Failed to create rendering surface: {e}");
                        continue;
                    }
                };

                let size = mode.size();
                let mode = Mode {
                    size: (size.0 as i32, size.1 as i32).into(),
                    refresh: (mode.vrefresh() * 1000) as i32,
                };

                let other_short_name;
                let interface_short_name = match conn_info.interface() {
                    Interface::DVII => "DVI-I",
                    Interface::DVID => "DVI-D",
                    Interface::DVIA => "DVI-A",
                    Interface::SVideo => "S-VIDEO",
                    Interface::DisplayPort => "DP",
                    Interface::HDMIA => "HDMI-A",
                    Interface::HDMIB => "HDMI-B",
                    Interface::EmbeddedDisplayPort => "eDP",
                    other => {
                        other_short_name = format!("{other:?}");
                        &other_short_name
                    }
                };

                let output_name = format!("{interface_short_name}-{}", conn_info.interface_id());

                let (phys_w, phys_h) = conn_info.size().unwrap_or_default();
                let props = PhysicalProperties {
                    size: (phys_w as i32, phys_h as i32).into(),
                    subpixel: Subpixel::Unknown,
                    make: "Smithay".into(),
                    model: "Generic DRM".into(),
                };
                let output = output_map.add(&output_name, props, mode);

                output.userdata().insert_if_missing(|| UdevOutputId {
                    crtc,
                    device_id: device.device_id(),
                });

                let surface_data = SurfaceData {
                    surface,
                    #[cfg(feature = "debug")]
                    fps: fps_ticker::Fps::default(),
                };
                entry.insert(Rc::new(RefCell::new(surface_data)));
                break 'enc;
            }
        }
    }

    backends
}

impl State<UdevData> {
    fn backends(&self) -> impl Iterator<Item = &'_ BackendData> {
        self.backend_data.backends.values()
    }

    fn device_added(&mut self, device_id: dev_t, path: PathBuf) {
        let flags = OFlag::O_RDWR | OFlag::O_CLOEXEC | OFlag::O_NOCTTY | OFlag::O_NONBLOCK;
        let fd = match self.backend_data.session.open(&path, flags) {
            Ok(v) => SessionFd(v),
            Err(_) => return,
        };

        macro_rules! attempt {
            ($category:literal, $e:expr) => {
                match $e {
                    Ok(v) => v,
                    Err(e) => {
                        warn!(
                            self.log,
                            "Skipping device {device_id:?} because of {} error: {e}", $category,
                        );
                        return;
                    }
                }
            };
        }

        let mut device = attempt!("drm", DrmDevice::new(fd.clone(), true, self.log.clone()));
        let gbm = attempt!("gbm", GbmDevice::new(fd));
        let egl = attempt!("egl display", EglDisplay::new(&gbm, self.log.clone()));
        let context = attempt!("egl context", EglContext::new(&egl, self.log.clone()));

        let renderer = Rc::new(RefCell::new(unsafe {
            Gles2Renderer::new(context, self.log.clone()).unwrap()
        }));

        if path.canonicalize().ok() == self.backend_data.gpu {
            info!(self.log, "Initializing EGL hwaccel via {path:?}");
            if renderer
                .borrow_mut()
                .bind_wl_display(&self.display.borrow())
                .is_ok()
            {
                info!(self.log, "EGL hwaccel enable");
            }
        }

        let surfaces = Rc::new(RefCell::new(scan_connectors(
            &mut device,
            &gbm,
            &mut renderer.borrow_mut(),
            &mut self.output_map.borrow_mut(),
            &self.backend_data.signaler,
            &self.log,
        )));

        let dev_id = device.device_id();
        let handle = self.handle.clone();
        let _restart_token = {
            let cb = move |signal: &_| match signal {
                SessionSignal::ActivateSession | SessionSignal::ActivateDevice { .. } => {
                    handle.insert_idle(move |state| state.render(dev_id, None));
                }
                _ => (),
            };
            self.backend_data.signaler.register(cb)
        };

        device.link(self.backend_data.signaler.clone());
        let event_dispatcher = {
            let cb = cb!(event, state => match event {
                DrmEvent::VBlank(crtc) => state.render(dev_id, Some(crtc)),
                DrmEvent::Error(e) => error!(state.log, "{e:?}"),
            });
            Dispatcher::new(device, cb)
        };
        let registration_token = self
            .handle
            .register_dispatcher(event_dispatcher.clone())
            .unwrap();

        trace!(self.log, "Backends: {:?}", surfaces.borrow().keys());
        for backend in surfaces.borrow().values() {
            trace!(self.log, "Scheduling frame");
            schedule_initial_render(
                backend.clone(),
                renderer.clone(),
                &self.handle,
                self.log.clone(),
            );
        }

        #[cfg(feature = "debug")]
        let fps_texture = {
            let png_data = std::io::Cursor::new(FPS_NUMBERS_PNG);
            let reader = image::io::Reader::with_format(png_data, ImageFormat::Png);
            let img = reader.decode().unwrap();
            import_bitmap(&mut renderer.borrow_mut(), &img.to_rgba8())
                .expect("Unable to upload FPS texture")
        };

        let backend = BackendData {
            _restart_token,
            surfaces,
            pointer_images: HashMap::new(),
            #[cfg(feature = "debug")]
            fps_texture,
            renderer,
            gbm,
            registration_token,
            event_dispatcher,
            dev_id,
        };
        self.backend_data.backends.insert(dev_id, backend);
    }

    fn device_changed(&mut self, device_id: dev_t) {
        let backend_data = match self.backend_data.backends.get_mut(&device_id) {
            Some(v) => v,
            None => return,
        };

        self.output_map.borrow_mut().retain(|output| {
            output
                .userdata()
                .get::<UdevOutputId>()
                .map(|id| id.device_id)
                != Some(device_id)
        });

        backend_data.surfaces.set(scan_connectors(
            &mut backend_data.event_dispatcher.as_source_mut(),
            &backend_data.gbm,
            &mut backend_data.renderer.borrow_mut(),
            &mut self.output_map.borrow_mut(),
            &self.backend_data.signaler,
            &self.log,
        ));

        for renderer in backend_data.surfaces.borrow().values() {
            schedule_initial_render(
                renderer.clone(),
                backend_data.renderer.clone(),
                &self.handle,
                self.log.clone(),
            );
        }
    }

    fn device_removed(&mut self, device_id: dev_t) {
        let backend_data = match self.backend_data.backends.remove(&device_id) {
            Some(v) => v,
            None => return,
        };

        backend_data.surfaces.borrow_mut().clear();
        debug!(self.log, "Surfaces dropped");

        self.output_map.borrow_mut().retain(|output| {
            output
                .userdata()
                .get::<UdevOutputId>()
                .map(|id| id.device_id)
                != Some(device_id)
        });

        self.handle.remove(backend_data.registration_token);
        let device = backend_data.event_dispatcher.into_source_inner();

        // don't use hardware acceleration anymore, if this was the primary gpu
        if device.dev_path().and_then(|path| path.canonicalize().ok()) == self.backend_data.gpu {
            backend_data.renderer.borrow_mut().unbind_wl_display();
        }
        debug!(self.log, "Dropping device");
    }

    fn render(&mut self, dev_id: u64, crtc: Option<crtc::Handle>) {
        let backend = match self.backend_data.backends.get_mut(&dev_id) {
            Some(backend) => backend,
            None => {
                error!(
                    self.log,
                    "Trying to render on non-existent backend {dev_id}",
                );
                return;
            }
        };

        let surfaces = backend.surfaces.borrow();

        let to_render = match crtc {
            Some(crtc) => Either::Left(
                surfaces
                    .get(&crtc)
                    .map(|surface| (crtc, surface))
                    .into_iter(),
            ),
            None => Either::Right(surfaces.iter().map(|(&c, s)| (c, s))),
        };

        for (crtc, surface) in to_render {
            // TODO: get scale from render surface
            let scale = 1;
            let frame = self
                .backend_data
                .pointer_image
                .get_image(scale, self.start_time.elapsed());

            let renderer = &mut backend.renderer.borrow_mut();

            let pointer_image = backend
                .pointer_images
                .entry(frame as *const Image)
                .or_insert_with(|| {
                    let image =
                        ImageBuffer::from_raw(frame.width, frame.height, &*frame.pixels_rgba)
                            .unwrap();
                    import_bitmap(renderer, &image).expect("Failed to import cursor bitmap")
                });

            let result = render_surface(
                &mut surface.borrow_mut(),
                renderer,
                backend.dev_id,
                crtc,
                &mut self.window_map.borrow_mut(),
                &self.output_map.borrow(),
                self.pointer_location,
                pointer_image,
                #[cfg(feature = "debug")]
                &backend.fps_texture,
                &self.drag_icon.borrow(),
                &mut self.cursor_status.borrow_mut(),
                &self.log,
            );

            if let Err(e) = result {
                warn!(self.log, "Error during rendering: {e:?}");
                let reschedule = match e {
                    SwapBuffersError::AlreadySwapped => false,
                    SwapBuffersError::TemporaryFailure(e) => match e.downcast_ref() {
                        Some(DrmError::DeviceInactive) => false,
                        Some(DrmError::Access { source, .. }) => {
                            !matches!(source, drm::SystemError::PermissionDenied)
                        }
                        _ => true,
                    },
                    SwapBuffersError::ContextLost(e) => panic!("Rendering loop lost: {e}"),
                };

                if reschedule {
                    debug!(self.log, "Rescheduling");
                    self.backend_data
                        .render_timer
                        .add_timeout(dur!(1 sec) / 60, (backend.dev_id, crtc));
                }
            } else {
                // TODO: only send drawn windows the frames callback
                // Send frame events so that client start drawing their next frame
                self.window_map
                    .borrow()
                    .send_frames(self.start_time.elapsed());
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn render_surface(
    surface: &mut SurfaceData,
    renderer: &mut Gles2Renderer,
    device_id: dev_t,
    crtc: crtc::Handle,
    window_map: &mut WindowMap,
    output_map: &OutputMap,
    pointer_location: Point<f64, Logical>,
    pointer_image: &Gles2Texture,
    #[cfg(feature = "debug")] fps_texture: &Gles2Texture,
    drag_icon: &Option<WlSurface>,
    cursor_status: &mut CursorImageStatus,
    log: &Logger,
) -> Result<(), SwapBuffersError> {
    surface.surface.frame_submitted()?;

    let output_id = UdevOutputId { device_id, crtc };
    let output = output_map.find(|o| o.userdata().get::<UdevOutputId>() == Some(&output_id));
    let (geometry, scale, mode) = match output {
        Some(o) => (o.geometry(), o.scale(), o.current_mode()),
        None => return Ok(()),
    };

    let (dmabuf, _) = surface.surface.next_buffer()?;
    renderer.bind(dmabuf)?;
    let rendering = |renderer: &mut _, frame: &mut Gles2Frame| -> Result<(), SwapBuffersError> {
        macro_rules! r_f {
            () => {
                (&mut *renderer, &mut *frame)
            };
        }

        render_layers_and_windows(renderer, frame, window_map, geometry, scale, log)?;

        if geometry.to_f64().contains(pointer_location) {
            let relative_ptr_location = pointer_location.to_i32_round() - geometry.loc;

            if let Some(surface) = drag_icon {
                if surface.as_ref().is_alive() {
                    draw_drag_icon(r_f!(), surface, relative_ptr_location, scale, log)?;
                }
            }

            if let CursorImageStatus::Image(surface) = cursor_status {
                if !surface.as_ref().is_alive() {
                    *cursor_status = CursorImageStatus::Default;
                }
            }

            if let CursorImageStatus::Image(surface) = cursor_status {
                draw_cursor(r_f!(), surface, relative_ptr_location, scale, log)?;
            } else {
                frame.render_texture_at(
                    pointer_image,
                    relative_ptr_location
                        .to_f64()
                        .to_physical(scale as f64)
                        .to_i32_round(),
                    1,
                    scale as f64,
                    Transform::Normal,
                    1.0,
                )?;
            }

            // Not sure why this got moved to inside the conditional.
            #[cfg(feature = "debug")]
            {
                let fps = &mut surface.fps;
                draw_fps(r_f!(), fps_texture, scale as f64, fps.avg().round() as u32)?;
                fps.tick();
            }
        }
        Ok(())
    };

    renderer.render(mode.size, Transform::Normal, rendering)??;
    surface.surface.queue_buffer()?;
    Ok(())
}

fn schedule_initial_render(
    surface: Rc<RefCell<SurfaceData>>,
    renderer: Rc<RefCell<Gles2Renderer>>,
    evt_handle: &LoopHandle<'static, State<UdevData>>,
    log: Logger,
) {
    let result = initial_render(
        &mut surface.borrow_mut().surface,
        &mut renderer.borrow_mut(),
    );
    match result {
        Err(SwapBuffersError::TemporaryFailure(e)) => {
            // TODO: don't reschedule after 3(?) retries
            warn!(log, "Failed to submit page_flip: {e}");
            let handle = evt_handle.clone();
            let cb = move |_: &mut _| schedule_initial_render(surface, renderer, &handle, log);
            evt_handle.insert_idle(cb);
        }
        Err(SwapBuffersError::ContextLost(e)) => panic!("Rendering loop lost: {e}"),
        _ => (),
    }
}

fn initial_render(
    surface: &mut RenderSurface,
    renderer: &mut Gles2Renderer,
) -> Result<(), SwapBuffersError> {
    let (dmabuf, _) = surface.next_buffer()?;
    renderer.bind(dmabuf)?;

    let rendering = |_: &mut _, frame: &mut Gles2Frame| frame.clear([0.8, 0.8, 0.9, 1.0], None);
    renderer.render((1, 1).into(), Transform::Normal, rendering)??;

    surface.queue_buffer()?;
    Ok(())
}
