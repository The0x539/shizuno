use std::os::unix::net::UnixStream;

use smithay::reexports::*;

use wayland_server::{protocol::wl_surface::WlSurface, Client};

use crate::{state::State, window_map::SurfaceTrait};

impl<B> State<B> {
    pub fn start_xwayland(&mut self) {
        todo!()
    }

    pub fn xwayland_ready(&mut self, _connection: UnixStream, _client: Client) {
        todo!()
    }

    pub fn xwayland_exited(&mut self) {
        todo!()
    }
}

#[derive(Clone, PartialEq)]
pub struct X11Surface {
    surface: WlSurface,
}

impl SurfaceTrait for X11Surface {
    fn alive(&self) -> bool {
        self.surface.as_ref().is_alive()
    }
    fn get_surface(&self) -> Option<&WlSurface> {
        if self.alive() {
            Some(&self.surface)
        } else {
            None
        }
    }
}
