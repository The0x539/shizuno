use std::cell::RefCell;
use std::collections::hash_map::{Entry, HashMap};
use std::collections::HashSet;
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::PathBuf;
use std::rc::Rc;

use smithay::backend::{
    allocator::{dmabuf::Dmabuf, Format},
    drm::{DevPath, DrmDevice, DrmError, DrmEvent, GbmBufferedSurface, GbmBufferedSurfaceError},
    egl::{EGLContext as EglContext, EGLDisplay as EglDisplay},
    libinput::{LibinputInputBackend, LibinputSessionInterface},
    renderer::{
        gles2::{Gles2Frame, Gles2Renderer, Gles2Texture},
        Bind, Frame, ImportDma, ImportEgl, Renderer,
    },
    session::{auto::AutoSession, Session, Signal as SessionSignal},
    udev::{primary_gpu, UdevBackend, UdevEvent},
    SwapBuffersError,
};
use smithay::desktop::space::{DynamicRenderElements, RenderError};
use smithay::desktop::Space;
use smithay::reexports::*;
use smithay::utils::Rectangle;
use smithay::utils::{
    signaling::{Linkable, SignalToken, Signaler},
    Logical, Point, Transform,
};
use smithay::wayland::{
    dmabuf::init_dmabuf_global,
    output::{Mode, Output, PhysicalProperties},
    seat::CursorImageStatus,
};

use calloop::{
    timer::{Timer, TimerHandle},
    Dispatcher, EventLoop, LoopHandle, RegistrationToken,
};
use drm::control::{connector, crtc, Device};
use either::Either;
use gbm::Device as GbmDevice;
use image::{ImageBuffer, ImageFormat};
use input::Libinput;
use nix::{fcntl::OFlag, libc::dev_t};
use slog::{crit, debug, error, info, trace, warn, Logger};
use sugars::dur;
use wayland_server::{
    protocol::{
        wl_output::{Subpixel, WlOutput},
        wl_surface::WlSurface,
    },
    DispatchData, Display, Global,
};
use xcursor::parser::Image;

use crate::render::render_output;
use crate::shell::fixup_positions;
use crate::util::PseudoCell;
use crate::{
    cursor::Cursor,
    drawing::*,
    state::{Backend, State},
};

#[derive(Copy, Clone)]
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

    fn reset_buffers(&mut self, output: &Output) {
        let id = try_or!(return, output.user_data().get::<UdevOutputId>());
        let gpu = try_or!(return, self.backends.get(&id.device_id));
        let surfaces = gpu.surfaces.borrow();
        let surface = try_or!(return, surfaces.get(&id.crtc));
        surface.borrow_mut().surface.reset_buffers();
    }
}

pub type RenderSurface = GbmBufferedSurface<GbmDevice<SessionFd>, SessionFd>;

struct SurfaceData {
    surface: RenderSurface,
    global: Option<Global<WlOutput>>,
    #[cfg(feature = "debug")]
    fps: fps_ticker::Fps,
}

impl Drop for SurfaceData {
    fn drop(&mut self) {
        if let Some(global) = self.global.take() {
            global.destroy();
        }
    }
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

    let udev_backend = match UdevBackend::new(&state.seat_name, log.clone()) {
        Ok(v) => v,
        Err(e) => {
            crit!(log, "Failed to initialize udev backend"; "error" => e);
            return;
        }
    };

    // libinput events
    {
        let interface = LibinputSessionInterface::from(state.backend_data.session.clone());
        let mut context = Libinput::new_with_udev(interface);
        context.udev_assign_seat(&state.seat_name).unwrap();
        let mut backend = LibinputInputBackend::new(context, log.clone());
        backend.link(session_signal);

        let cb = cb!(event, state => state.process_input_event(event));
        handle.insert_source(backend, cb).unwrap();
    }

    // session events
    handle.insert_source(notifier, cb!()).unwrap();

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

    // udev events
    {
        let cb = cb!(event, state => match event {
            UdevEvent::Added { device_id, path } => state.device_added(device_id, path),
            UdevEvent::Changed { device_id } => state.device_changed(device_id),
            UdevEvent::Removed { device_id } => state.device_removed(device_id),
        });
        handle.insert_source(udev_backend, cb).unwrap();
    };

    state.start_xwayland();

    while state.running.get() {
        // TODO: refresh rate stuff
        let dur = dur!(16 milli);
        let dispatch_result = event_loop.dispatch(dur, &mut state);

        if dispatch_result.is_err() {
            state.running.set(false);
        } else {
            state.space.borrow_mut().refresh();
            state.popups.borrow_mut().cleanup();
            display.borrow_mut().flush_clients(&mut state);
        }
    }
}

struct OutputName<'a>(&'a connector::Info);
impl std::fmt::Display for OutputName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (iface, id) = (self.0.interface(), self.0.interface_id());

        use connector::Interface as Iface;
        let short_name = match iface {
            Iface::DVII => "DVI-I",
            Iface::DVID => "DVI-D",
            Iface::DVIA => "DVI-A",
            Iface::SVideo => "S-VIDEO",
            Iface::DisplayPort => "DP",
            Iface::HDMIA => "HDMI-A",
            Iface::HDMIB => "HDMI-B",
            Iface::EmbeddedDisplayPort => "eDP",
            _ => return write!(f, "{iface:?}-{id}"),
        };
        write!(f, "{short_name}-{id}")
    }
}

fn scan_connectors(
    device: &DrmDevice<SessionFd>,
    gbm: &GbmDevice<SessionFd>,
    renderer: &mut Gles2Renderer,
    display: &mut Display,
    space: &mut Space,
    signaler: &Signaler<SessionSignal>,
    log: &Logger,
) -> HashMap<crtc::Handle, Rc<RefCell<SurfaceData>>> {
    let res_handles = device.resource_handles().unwrap();

    let formats =
        Bind::<Dmabuf>::supported_formats(renderer).expect("dmabuf renderer without formats");

    let mut backends = HashMap::new();

    'conn: for conn in res_handles.connectors() {
        let conn_info = device.get_connector(*conn).unwrap();
        if conn_info.state() != connector::State::Connected {
            continue;
        }

        let name = OutputName(&conn_info);
        info!(log, "Connected: {name}");

        for enc in conn_info.encoders() {
            let enc_handle = try_or!(continue, enc.as_ref().copied());
            let enc_info = try_or!(continue, device.get_encoder(enc_handle).ok());

            for crtc in res_handles.filter_crtcs(enc_info.possible_crtcs()) {
                let entry = match backends.entry(crtc) {
                    Entry::Vacant(v) => v,
                    Entry::Occupied(_) => continue,
                };

                info!(log, "Trying to setup connector {name} with crtc {crtc:?}");

                match setup_connector(
                    device, gbm, display, space, signaler, log, crtc, &conn_info, &formats,
                ) {
                    Ok(data) => {
                        entry.insert(Rc::new(RefCell::new(data)));
                        continue 'conn;
                    }
                    Err(GbmBufferedSurfaceError::DrmError(e)) => {
                        warn!(log, "Failed to create drm surface: {e}");
                    }
                    Err(e) => {
                        warn!(log, "Failed to create rendering surface: {e}");
                    }
                }
            }
        }
    }

    backends
}

#[allow(clippy::too_many_arguments)]
fn setup_connector(
    device: &DrmDevice<SessionFd>,
    gbm: &GbmDevice<SessionFd>,
    display: &mut Display,
    space: &mut Space,
    signaler: &Signaler<SessionSignal>,
    log: &Logger,

    crtc: crtc::Handle,
    conn_info: &connector::Info,
    formats: &HashSet<Format>,
) -> Result<SurfaceData, GbmBufferedSurfaceError<std::io::Error>> {
    // TODO: honor configured mode
    let mode = conn_info.modes()[0];

    let mut surface = device.create_surface(crtc, mode, &[conn_info.handle()])?;
    surface.link(signaler.clone());

    let surface = GbmBufferedSurface::new(surface, gbm.clone(), formats.clone(), log.clone())?;

    let size = mode.size();
    let mode = Mode {
        size: (size.0 as i32, size.1 as i32).into(),
        refresh: (mode.vrefresh() * 1000) as i32,
    };

    let (phys_w, phys_h) = conn_info.size().unwrap_or_default();
    let props = PhysicalProperties {
        size: (phys_w as i32, phys_h as i32).into(),
        subpixel: Subpixel::Unknown,
        make: "Smithay".into(),
        model: "Generic DRM".into(),
    };

    let name = OutputName(conn_info).to_string();
    let (output, global) = Output::new(display, name.clone(), props, None);

    // TODO: arrangements and what not
    let width = space
        .outputs()
        .map(|o| space.output_geometry(o).unwrap().size.w)
        .sum::<i32>();
    let position = (width, 0).into();
    output.change_current_state(Some(mode), None, None, Some(position));
    output.set_preferred(mode);

    let scale = crate::config::get().display_scale(&name);
    space.map_output(&output, scale, position);

    output.user_data().insert_if_missing(|| UdevOutputId {
        crtc,
        device_id: device.device_id(),
    });

    Ok(SurfaceData {
        surface,
        global: Some(global),
        #[cfg(feature = "debug")]
        fps: fps_ticker::Fps::default(),
    })
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

        let mut device = attempt!("drm", DrmDevice::new(fd, true, self.log.clone()));
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
            &device,
            &gbm,
            &mut renderer.borrow_mut(),
            &mut self.display.borrow_mut(),
            &mut self.space.borrow_mut(),
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

        let mut space = self.space.borrow_mut();
        unmap_outputs(&mut space, device_id);

        backend_data.surfaces.set(scan_connectors(
            &backend_data.event_dispatcher.as_source_ref(),
            &backend_data.gbm,
            &mut backend_data.renderer.borrow_mut(),
            &mut self.display.borrow_mut(),
            &mut self.space.borrow_mut(),
            &self.backend_data.signaler,
            &self.log,
        ));

        fixup_positions(&mut space);

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

        let mut space = self.space.borrow_mut();
        unmap_outputs(&mut space, device_id);

        fixup_positions(&mut space);

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
                &mut self.space.borrow_mut(),
                self.pointer_location,
                pointer_image,
                #[cfg(feature = "debug")]
                &backend.fps_texture,
                &self.drag_icon.borrow(),
                &mut self.cursor_status.borrow_mut(),
                &self.log,
            );

            let reschedule = match result {
                Ok(has_rendered) => !has_rendered,
                Err(e) => {
                    warn!(self.log, "Error during rendering: {e:?}");
                    match e {
                        SwapBuffersError::AlreadySwapped => false,
                        SwapBuffersError::TemporaryFailure(e) => match e.downcast_ref() {
                            Some(DrmError::DeviceInactive) => false,
                            Some(DrmError::Access { source, .. }) => {
                                !matches!(source, drm::SystemError::PermissionDenied)
                            }
                            _ => true,
                        },
                        SwapBuffersError::ContextLost(e) => panic!("Rendering loop lost: {e}"),
                    }
                }
            };

            if reschedule {
                self.backend_data
                    .render_timer
                    .add_timeout(dur!(1 sec) / 60, (backend.dev_id, crtc));
            }

            self.space
                .borrow()
                .send_frames(false, self.start_time.elapsed().as_millis() as u32);
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn render_surface(
    surface: &mut SurfaceData,
    renderer: &mut Gles2Renderer,
    device_id: dev_t,
    crtc: crtc::Handle,
    space: &mut Space,
    pointer_location: Point<f64, Logical>,
    pointer_image: &Gles2Texture,
    #[cfg(feature = "debug")] fps_texture: &Gles2Texture,
    drag_icon: &Option<WlSurface>,
    cursor_status: &mut CursorImageStatus,
    log: &Logger,
) -> Result<bool, SwapBuffersError> {
    surface.surface.frame_submitted()?;

    let output = match space
        .outputs()
        .find(|o| o.user_data().get::<UdevOutputId>() == Some(&UdevOutputId { device_id, crtc }))
    {
        Some(o) => o.clone(),
        None => return Ok(true), // somehow called with an invalid output
    };

    let geometry = space.output_geometry(&output).unwrap();
    let (dmabuf, age) = surface.surface.next_buffer()?;
    renderer.bind(dmabuf)?;

    let mut elements = Vec::<DynamicRenderElements<_>>::new();

    // set cursor
    if geometry.to_f64().contains(pointer_location) {
        let relative_ptr_location = pointer_location.to_i32_round() - geometry.loc;
        if let Some(wl_surface) = drag_icon {
            if wl_surface.as_ref().is_alive() {
                elements.push(Box::new(draw_drag_icon_new(
                    wl_surface.clone(),
                    relative_ptr_location,
                    log,
                )));
            }
        }

        if let CursorImageStatus::Image(surface) = cursor_status {
            if !surface.as_ref().is_alive() {
                *cursor_status = CursorImageStatus::Default;
            }
        }

        if let CursorImageStatus::Image(surface) = cursor_status {
            elements.push(Box::new(draw_cursor_new(
                surface.clone(),
                relative_ptr_location,
                log,
            )));
        } else {
            elements.push(Box::new(PointerElement::new(
                pointer_image.clone(),
                relative_ptr_location,
            )));
        }
    }

    // Not sure why this got moved to inside the conditional.
    #[cfg(feature = "debug")]
    {
        let fps_val = surface.fps.avg().round() as u32;
        elements.push(Box::new(draw_fps_new(fps_texture, fps_val)));
        surface.fps.tick();
    }

    // TODO: pass damage rectangles inside an AtomicCommitRequest
    match render_output(&output, space, renderer, age.into(), &elements, log) {
        Ok(Some(_)) => {
            surface.surface.queue_buffer()?;
            Ok(true)
        }
        Ok(None) => Ok(false),
        Err(RenderError::Rendering(e)) => Err(e.into()),
        Err(_) => unreachable!(),
    }
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

    let damage = &[Rectangle::from_loc_and_size((0, 0), (1, 1))];
    let rendering = |_: &mut _, frame: &mut Gles2Frame| frame.clear(CLEAR_COLOR, damage);
    renderer.render((1, 1).into(), Transform::Normal, rendering)??;

    surface.queue_buffer()?;
    surface.reset_buffers();
    Ok(())
}

fn unmap_outputs(space: &mut Space, device_id: dev_t) {
    let to_unmap = space
        .outputs()
        .filter(|o| {
            o.user_data()
                .get::<UdevOutputId>()
                .map_or(false, |id| id.device_id == device_id)
        })
        .cloned()
        .collect::<Vec<_>>();

    for output in to_unmap {
        space.unmap_output(&output);
    }
}
