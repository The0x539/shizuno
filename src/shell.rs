use std::{any::Any, cell::RefCell, rc::Rc, time::Duration};

use smithay::reexports::*;
use smithay::utils::{Logical, Point, Size};
use smithay::wayland::compositor::SurfaceAttributes;

use slog::Logger;
use wayland_server::{protocol::wl_buffer::WlBuffer, Display};

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

pub struct SurfaceData {
    pub buffer: Option<WlBuffer>,
    pub texture: Option<Box<dyn Any + 'static>>,
    pub buffer_scale: i32,
}

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
