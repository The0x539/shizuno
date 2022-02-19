use std::time::Duration;

use smithay::reexports::*;
use smithay::utils::{Logical, Point, Rectangle};
use smithay::wayland::shell::{legacy::ShellSurface, xdg::ToplevelSurface};

use wayland_server::protocol::wl_surface::WlSurface;

use crate::xwayland::X11Surface;

#[derive(Clone)]
pub enum Kind {
    Xdg(ToplevelSurface),
    Wl(ShellSurface),
    X11(X11Surface),
}

impl Kind {
    pub fn get_surface(&self) -> Option<&WlSurface> {
        todo!()
    }
}

pub struct WindowMap;

impl WindowMap {
    pub fn with_windows_from_bottom_to_top<F>(&self, _f: F)
    where
        F: FnMut(&Kind, Point<i32, Logical>, &Rectangle<i32, Logical>),
    {
        todo!()
    }

    pub fn refresh(&mut self) {
        todo!()
    }

    pub fn clear(&mut self) {
        todo!()
    }

    pub fn find(&self, _surface: &WlSurface) -> Option<Kind> {
        todo!()
    }

    pub fn send_frames(&self, _time: Duration) {
        todo!()
    }

    pub fn location(&self, toplevel: &Kind) -> Option<Point<i32, Logical>> {
        todo!()
    }

    pub fn set_location(&mut self, toplevel: &Kind, location: Point<i32, Logical>) {}
}
