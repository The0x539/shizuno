use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use smithay::backend::{
    libinput::{LibinputInputBackend, LibinputSessionInterface},
    renderer::{gles2::Gles2Renderer, ImportDma},
    session::{auto::AutoSession, Session},
    udev::{primary_gpu, UdevBackend, UdevEvent},
};
use smithay::reexports::*;
use smithay::utils::signaling::Linkable;
use smithay::wayland::dmabuf::init_dmabuf_global;

use calloop::{timer::Timer, EventLoop};
use drm::control::crtc;
use input::Libinput;
use nix::libc::dev_t;
use slog::{crit, Logger};
use sugars::dur;
use wayland_server::{DispatchData, Display};

use crate::state::{Backend, State};

pub struct UdevData {
    session: AutoSession,
    gpu: Option<PathBuf>,
    backends: HashMap<dev_t, BackendData>,
}

impl Backend for UdevData {
    fn seat_name(&self) -> String {
        self.session.seat()
    }
}

struct BackendData {
    renderer: Rc<RefCell<Gles2Renderer>>,
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

    fn render(&mut self, _dev_id: u64, _crtc: Option<crtc::Handle>) {
        todo!()
    }
}
