use std::cell::RefCell;
use std::collections::HashMap;
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::PathBuf;
use std::rc::Rc;

use smithay::backend::{
    drm::{DrmError, GbmBufferedSurface},
    libinput::{LibinputInputBackend, LibinputSessionInterface},
    renderer::{
        gles2::{Gles2Frame, Gles2Renderer, Gles2Texture},
        Bind, Frame, ImportDma, Renderer, Transform,
    },
    session::{auto::AutoSession, Session},
    udev::{primary_gpu, UdevBackend, UdevEvent},
    SwapBuffersError,
};
use smithay::reexports::*;
use smithay::utils::{signaling::Linkable, Logical, Point};
use smithay::wayland::{dmabuf::init_dmabuf_global, seat::CursorImageStatus};

use calloop::{
    timer::{Timer, TimerHandle},
    EventLoop,
};
use drm::control::crtc;
use either::Either;
use image::ImageBuffer;
use input::Libinput;
use nix::libc::dev_t;
use slog::{crit, debug, error, warn, Logger};
use sugars::dur;
use wayland_server::{protocol::wl_surface::WlSurface, DispatchData, Display};
use xcursor::parser::Image;

use crate::{
    cursor::Cursor,
    drawing::*,
    output_map::OutputMap,
    state::{Backend, State},
    window_map::WindowMap,
};

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
    session: AutoSession,
    _gpu: Option<PathBuf>,
    backends: HashMap<dev_t, BackendData>,
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
    surfaces: Rc<RefCell<HashMap<crtc::Handle, Rc<RefCell<SurfaceData>>>>>,
    pointer_images: HashMap<*const Image, Gles2Texture>,
    #[cfg(feature = "debug")]
    fps_texture: Gles2Texture,
    renderer: Rc<RefCell<Gles2Renderer>>,
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
        _gpu: gpu,
        backends: HashMap::new(),
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

impl State<UdevData> {
    fn backends(&self) -> impl Iterator<Item = &'_ BackendData> {
        self.backend_data.backends.values()
    }

    fn device_added(&mut self, _device_id: dev_t, _path: PathBuf) {
        todo!()
    }

    fn device_changed(&mut self, _device_id: dev_t) {
        todo!()
    }

    fn device_removed(&mut self, _device_id: dev_t) {
        todo!()
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
                .entry(Rc::as_ptr(&frame))
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
                &pointer_image,
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
}

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

    let dmabuf = surface.surface.next_buffer()?;
    renderer.bind(dmabuf)?;
    let rendering = |renderer: &mut _, frame: &mut Gles2Frame| -> Result<(), SwapBuffersError> {
        frame.clear([0.8, 0.8, 0.9, 1.0])?;
        macro_rules! r_f {
            () => {
                (&mut *renderer, &mut *frame)
            };
        }
        draw_windows(r_f!(), window_map, geometry, scale, log)?;

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
        }

        #[cfg(feature = "debug")]
        {
            let fps = &mut surface.fps;
            draw_fps(r_f!(), fps_texture, scale as f64, fps.avg().round() as u32)?;
            fps.tick();
        }
        Ok(())
    };

    renderer.render(mode.size, Transform::Flipped180, rendering)??;
    surface.surface.queue_buffer()?;
    Ok(())
}
