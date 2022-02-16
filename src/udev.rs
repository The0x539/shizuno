use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use smithay::reexports::*;
use smithay::{
    backend::{
        session::{auto::AutoSession, Session},
        udev::primary_gpu,
    },
    wayland::output::Output,
};

use calloop::EventLoop;
use slog::{crit, Logger};
use wayland_server::Display;

use crate::state::{BackendData, State};

#[allow(dead_code)]
struct UdevData {
    session: AutoSession,
    gpu: Option<PathBuf>,
}

impl BackendData for UdevData {
    fn seat_name(&self) -> String {
        todo!()
    }

    fn reset_buffers(&mut self, _output: &Output) {
        todo!()
    }
}

pub fn run(log: Logger) {
    let event_loop = EventLoop::try_new().unwrap();
    let display = Rc::new(RefCell::new(Display::new()));

    let (session, _notifier) = match AutoSession::new(log.clone()) {
        Some(v) => v,
        None => {
            crit!(log, "Could not initialize a session");
            return;
        }
    };

    let gpu = primary_gpu(&session.seat()).unwrap_or(None);
    let data = UdevData { session, gpu };

    let _state = State::init(display, event_loop.handle(), data, log.clone(), true);
}
