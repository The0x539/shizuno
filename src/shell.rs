use std::{cell::RefCell, rc::Rc, time::Duration};

use smithay::reexports::*;
use smithay::utils::{Logical, Point, Size};
use smithay::wayland::compositor::SurfaceAttributes;

use slog::Logger;
use wayland_server::Display;

use crate::{output_map::OutputMap, window_map::WindowMap};

pub struct ShellHandles {
    pub window_map: Rc<RefCell<WindowMap>>,
    pub output_map: Rc<RefCell<OutputMap>>,
}

impl ShellHandles {
    pub fn init<B>(_display: Rc<RefCell<Display>>, _log: Logger) -> Self {
        todo!()
    }
}

pub struct SurfaceData;

impl SurfaceData {
    pub fn size(&self) -> Option<Size<i32, Logical>> {
        todo!()
    }

    pub fn contains_point(&self, _attrs: &SurfaceAttributes, _point: Point<f64, Logical>) -> bool {
        todo!()
    }

    pub fn send_frame(_attrs: &mut SurfaceAttributes, _time: Duration) {
        todo!()
    }
}
