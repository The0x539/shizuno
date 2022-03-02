use std::{cell::RefCell, collections::HashMap, os::unix::net::UnixStream, rc::Rc, sync::Arc};

use smithay::desktop::{Kind as SurfaceKind, Space, Window, X11Surface};
use smithay::reexports::*;
use smithay::utils::{x11rb::X11Source, Logical, Point};
use smithay::wayland::compositor::give_role;

use slog::{debug, error, info, Logger};
use wayland_server::{protocol::wl_surface::WlSurface, Client};
use x11rb::{
    connection::Connection,
    errors::ReplyOrIdError,
    protocol::{
        composite::{ConnectionExt as _, Redirect},
        xproto::{
            ChangeWindowAttributesAux, ConfigWindow, ConfigureWindowAux, ConnectionExt as _,
            EventMask, Window as X11Window, WindowClass,
        },
        Event,
    },
    rust_connection::{DefaultStream, RustConnection},
};

use crate::state::State;

impl<B: 'static> State<B> {
    pub fn start_xwayland(&mut self) {
        if let Err(e) = self.xwayland.start() {
            error!(self.log, "Failed to start XWayland: {e}");
        }
    }

    pub fn xwayland_ready(&mut self, connection: UnixStream, client: Client) {
        let (wm, source) =
            X11State::start_wm(connection, self.space.clone(), self.log.clone()).unwrap();
        let wm = Rc::new(RefCell::new(wm));
        client.data_map().insert_if_missing(|| wm.clone());
        let log = self.log.clone();
        let f = cb!(event, _ => {
            if let Err(e) = wm.borrow_mut().handle_event(event, &client) {
            error!(log, "Error while handling X11 event: {e}");
        }
        });
        self.handle.insert_source(source, f).unwrap();
    }

    pub fn xwayland_exited(&mut self) {
        error!(self.log, "Xwayland crashed");
    }
}

x11rb::atom_manager! {
    Atoms: AtomsCookie {
        WM_S0,
        WL_SURFACE_ID,
        CLOSE_CONNECTION,
    }
}

struct X11State {
    conn: Arc<RustConnection>,
    atoms: Atoms,
    log: Logger,
    unpaired_surfaces: HashMap<u32, (X11Window, Point<i32, Logical>)>,
    space: Rc<RefCell<Space>>,
}

impl X11State {
    fn start_wm(
        connection: UnixStream,
        space: Rc<RefCell<Space>>,
        log: Logger,
    ) -> Result<(Self, X11Source), Box<dyn std::error::Error>> {
        let screen = 0;
        let stream = DefaultStream::from_unix_stream(connection)?;
        let conn = RustConnection::connect_to_stream(stream, screen)?;
        let atoms = Atoms::new(&conn)?.reply()?;

        let screen = &conn.setup().roots[0];

        let attrs = ChangeWindowAttributesAux::new().event_mask(EventMask::SUBSTRUCTURE_REDIRECT);
        conn.change_window_attributes(screen.root, &attrs)?;

        let win = conn.generate_id()?;
        conn.create_window(
            screen.root_depth,
            win,
            screen.root,
            0,
            0,
            1,
            1,
            0,
            WindowClass::INPUT_OUTPUT,
            x11rb::COPY_FROM_PARENT,
            &Default::default(),
        )?;
        conn.set_selection_owner(win, atoms.WM_S0, x11rb::CURRENT_TIME)?;

        conn.composite_redirect_subwindows(screen.root, Redirect::MANUAL)?;

        conn.flush()?;

        let conn = Arc::new(conn);
        let wm = Self {
            conn: conn.clone(),
            atoms,
            log: log.clone(),
            unpaired_surfaces: Default::default(),
            space,
        };

        Ok((wm, X11Source::new(conn, win, atoms.CLOSE_CONNECTION, log)))
    }

    fn handle_event(&mut self, event: Event, client: &Client) -> Result<(), ReplyOrIdError> {
        debug!(self.log, "X11: Got event {event:?}");

        match event {
            Event::ConfigureRequest(r) => {
                let mut aux = ConfigureWindowAux::new();
                macro_rules! field {
                    ($field:ident, $flag:ident) => {
                        if r.value_mask & u16::from(ConfigWindow::$flag) != 0 {
                            aux.$field = Some(r.$field.try_into().unwrap());
                        }
                    };
                }

                field!(stack_mode, STACK_MODE);
                field!(sibling, SIBLING);
                field!(x, X);
                field!(y, Y);
                field!(width, WIDTH);
                field!(height, HEIGHT);
                field!(border_width, BORDER_WIDTH);

                self.conn.configure_window(r.window, &aux)?;
            }
            Event::MapRequest(r) => {
                self.conn.map_window(r.window)?;
            }
            Event::ClientMessage(msg) if msg.type_ == self.atoms.WL_SURFACE_ID => {
                let win = msg.window;
                let location = match self.conn.get_geometry(msg.window)?.reply() {
                    Ok(geo) => (geo.x as i32, geo.y as i32).into(),
                    Err(e) => {
                        error!(
                            self.log,
                            "Failed to get geometry for {win:x}, perhaps the window was already destroyed?";
                            "err" => format!("{e:?}"),
                        );
                        (0, 0).into()
                    }
                };

                let id = msg.data.as_data32()[0];
                let surface = client.get_resource::<WlSurface>(id);
                info!(
                    self.log,
                    "X11 surface {win:x?} corresponds to WlSurface {id:x} = {surface:?}",
                );

                if let Some(surface) = surface {
                    self.new_window(msg.window, surface, location)
                } else {
                    self.unpaired_surfaces.insert(id, (msg.window, location));
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn new_window(&mut self, window: X11Window, surface: WlSurface, location: Point<i32, Logical>) {
        debug!(self.log, "Matched X11 surface {window:x?} to {surface:x?}");

        if give_role(&surface, "x11_surface").is_err() {
            error!(self.log, "Surface {surface:x?} already has a role‽");
            return;
        }

        let surface = X11Surface { surface };
        self.space
            .borrow_mut()
            .map_window(&Window::new(SurfaceKind::X11(surface)), location, true);
    }
}

pub fn commit_hook(surface: &WlSurface) {
    let client = try_or!(return, surface.as_ref().client());
    let x11 = try_or!(return, client.data_map().get::<Rc<RefCell<X11State>>>());
    let mut inner = x11.borrow_mut();
    let id = surface.as_ref().id();
    let (window, location) = try_or!(return, inner.unpaired_surfaces.remove(&id));
    inner.new_window(window, surface.clone(), location);
}
