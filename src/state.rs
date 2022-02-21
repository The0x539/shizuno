use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::rc::Rc;
use std::time::{Duration, Instant};

use smithay::reexports::*;
use smithay::utils::{Logical, Point};
use smithay::wayland::{
    data_device::{
        default_action_chooser, init_data_device, set_data_device_focus, DataDeviceEvent,
    },
    output::xdg::init_xdg_output_manager,
    seat::{CursorImageStatus, KeyboardHandle, PointerHandle, Seat},
    shm,
    tablet_manager::{init_tablet_manager_global, TabletSeatTrait},
};
use smithay::xwayland::{XWayland, XWaylandEvent};

use calloop::{generic::Generic, Interest, LoopHandle, Mode, PostAction};
use slog::{error, info, Logger};
use wayland_server::{protocol::wl_surface::WlSurface, Display};

use crate::{output_map::OutputMap, shell::ShellHandles, util::PseudoCell, window_map::WindowMap};

pub trait Backend {
    fn seat_name(&self) -> String;
}

#[allow(dead_code)]
pub struct State<B> {
    pub backend_data: B,
    socket_name: Option<String>,
    pub running: Cell<bool>,
    pub display: Rc<RefCell<Display>>,
    pub handle: LoopHandle<'static, Self>,
    pub window_map: Rc<RefCell<WindowMap>>,
    pub output_map: Rc<RefCell<OutputMap>>,
    pub drag_icon: Rc<RefCell<Option<WlSurface>>>,
    pub log: Logger,

    pub pointer: PointerHandle,
    pub keyboard: KeyboardHandle,
    pub suppressed_keys: HashSet<u32>,
    pub pointer_location: Point<f64, Logical>,
    pub seat_name: String,
    pub cursor_status: Rc<RefCell<CursorImageStatus>>,
    pub seat: Seat,
    pub start_time: Instant,

    pub xwayland: XWayland<Self>,
}

impl<B: Backend + 'static> State<B> {
    pub fn init(
        display_rc: Rc<RefCell<Display>>,
        handle: LoopHandle<'static, Self>,
        backend_data: B,
        log: Logger,
        listen_on_socket: bool,
    ) -> Self {
        let display = display_rc.clone();
        let display = &mut display.borrow_mut();

        {
            let event_source = Generic::from_fd(display.get_poll_fd(), Interest::READ, Mode::Level);
            let cb = cb!(_, state => {
                let display = state.display.clone();
                let dispatch_result = display.borrow_mut().dispatch(Duration::ZERO, state);
                if let Err(e) = dispatch_result {
                    error!(state.log, "I/O error on the Wayland display: {e}");
                    state.running.set(false);
                    Err(e)
                } else {
                    Ok(PostAction::Continue)
                }
            });
            handle
                .insert_source(event_source, cb)
                .expect("failed to init wayland event source");
        }

        shm::init_shm_global(display, vec![], log.clone());

        let ShellHandles {
            window_map,
            output_map,
        } = ShellHandles::init::<B>(display_rc.clone(), log.clone());

        init_xdg_output_manager(display, log.clone());

        let socket_name = if listen_on_socket {
            let name = display.add_socket_auto().unwrap().into_string().unwrap();
            info!(log, "Listening on wayland socket"; "name" => &name);
            std::env::set_var("WAYLAND_DISPLAY", &name);
            Some(name)
        } else {
            None
        };

        let drag_icon: Rc<RefCell<_>> = Default::default();

        {
            let drag_icon = drag_icon.clone();
            let cb = move |event| match event {
                DataDeviceEvent::DnDStarted { icon, .. } => drag_icon.set(icon),
                DataDeviceEvent::DnDDropped => drag_icon.set(None),
                _ => (),
            };
            init_data_device(display, cb, default_action_chooser, log.clone());
        }

        let seat_name = backend_data.seat_name();
        let (mut seat, _) = Seat::new(display, seat_name.clone(), log.clone());

        let cursor_status = Rc::new(RefCell::new(CursorImageStatus::Default));

        let pointer = {
            let cursor_status = cursor_status.clone();
            let cb = move |status| cursor_status.set(status);
            seat.add_pointer(cb)
        };

        {
            init_tablet_manager_global(display);
            let cursor_status = cursor_status.clone();
            let cb = move |_tool: &_, status| cursor_status.set(status);
            seat.tablet_seat().on_cursor_surface(cb);
        }

        let keyboard = {
            let cb = |seat: &_, focus: Option<&WlSurface>| {
                set_data_device_focus(seat, focus.and_then(|s| s.as_ref().client()))
            };
            seat.add_keyboard(Default::default(), 200, 25, cb)
                .expect("Failed to initialize the keyboard")
        };

        let xwayland = {
            let (xwayland, channel) =
                XWayland::new(handle.clone(), display_rc.clone(), log.clone());
            let cb = |event, _: &mut _, state: &mut Self| match event {
                XWaylandEvent::Ready { connection, client } => {
                    state.xwayland_ready(connection, client)
                }
                XWaylandEvent::Exited => state.xwayland_exited(),
            };
            if let Err(e) = handle.insert_source(channel, cb) {
                error!(
                    log,
                    "Failed to insert the XWaylandSource into the event loop: {e}",
                );
            }
            xwayland
        };

        Self {
            backend_data,
            socket_name,
            running: Default::default(),
            display: display_rc,
            handle,
            window_map,
            output_map,
            drag_icon,
            log,

            pointer,
            keyboard,
            suppressed_keys: HashSet::new(),
            pointer_location: Default::default(),
            cursor_status,
            seat_name,
            seat,
            start_time: Instant::now(),

            xwayland,
        }
    }
}
