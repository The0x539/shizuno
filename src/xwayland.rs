use std::os::unix::net::UnixStream;

use smithay::reexports::*;

use wayland_server::Client;

use crate::state::State;

impl<B> State<B> {
    pub fn xwayland_ready(&mut self, _connection: UnixStream, _client: Client) {
        todo!()
    }

    pub fn xwayland_exited(&mut self) {
        todo!()
    }
}
