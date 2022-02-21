use std::{io, os::unix::io::AsRawFd, rc::Rc};

use smithay::reexports::*;

use calloop::{
    generic::{Fd, Generic},
    EventSource, Interest, Mode, Poll, PostAction, Readiness, Token, TokenFactory,
};
use x11rb::{
    connection::Connection, errors::ReplyOrIdError, protocol::Event,
    rust_connection::RustConnection,
};

pub struct X11Source {
    connection: Rc<RustConnection>,
    generic: Generic<Fd>,
}

impl X11Source {
    pub fn new(connection: Rc<RustConnection>) -> Self {
        let fd = Fd(connection.stream().as_raw_fd());
        let generic = Generic::new(fd, Interest::READ, Mode::Level);
        Self {
            connection,
            generic,
        }
    }
}

impl EventSource for X11Source {
    type Event = Vec<Event>;
    type Metadata = ();
    type Ret = Result<(), ReplyOrIdError>;

    fn process_events<F: FnMut(Self::Event, &mut Self::Metadata) -> Self::Ret>(
        &mut self,
        readiness: Readiness,
        token: Token,
        mut callback: F,
    ) -> io::Result<PostAction> {
        fn io_err<E: std::error::Error + Send + Sync + 'static>(err: E) -> io::Error {
            io::Error::new(io::ErrorKind::Other, err)
        }

        let f = |_, _: &mut _| {
            let mut events = Vec::new();
            while let Some(event) = self.connection.poll_for_event().map_err(io_err)? {
                events.push(event);
            }
            if !events.is_empty() {
                callback(events, &mut ()).map_err(io_err)?;
            }
            self.connection.flush().map_err(io_err)?;
            Ok(PostAction::Continue)
        };

        self.generic.process_events(readiness, token, f)
    }

    fn register(&mut self, poll: &mut Poll, factory: &mut TokenFactory) -> io::Result<()> {
        self.generic.register(poll, factory)
    }

    fn reregister(&mut self, poll: &mut Poll, factory: &mut TokenFactory) -> io::Result<()> {
        self.generic.reregister(poll, factory)
    }

    fn unregister(&mut self, poll: &mut Poll) -> io::Result<()> {
        self.generic.unregister(poll)
    }
}
