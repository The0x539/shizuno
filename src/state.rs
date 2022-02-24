use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use smithay::reexports::*;
use smithay::utils::{Logical, Point};
use smithay::wayland::{
    data_device::{
        default_action_chooser, init_data_device, set_data_device_focus, DataDeviceEvent,
    },
    output::xdg::init_xdg_output_manager,
    seat::{CursorImageStatus, KeyboardHandle, PointerHandle, Seat},
    shell::xdg::decoration::{init_xdg_decoration_manager, XdgDecorationRequest},
    shm,
    tablet_manager::{init_tablet_manager_global, TabletSeatTrait},
    xdg_activation::{init_xdg_activation_global, XdgActivationEvent, XdgActivationState},
};
use smithay::xwayland::{XWayland, XWaylandEvent};

use calloop::{generic::Generic, Interest, LoopHandle, Mode, PostAction};
use slog::{error, info, Logger};
use wayland_protocols::unstable::xdg_decoration;
use wayland_server::{protocol::wl_surface::WlSurface, DispatchData, Display};

use crate::{output_map::OutputMap, shell::ShellHandles, util::PseudoCell, window_map::WindowMap};

pub trait Backend {
    fn seat_name(&self) -> String;
}

pub struct State<B> {
    pub backend_data: B,
    pub socket_name: Option<String>,
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
        display: Rc<RefCell<Display>>,
        handle: LoopHandle<'static, Self>,
        backend_data: B,
        log: Logger,
        listen_on_socket: bool,
    ) -> Self {
        {
            let event_source =
                Generic::from_fd(display.borrow().get_poll_fd(), Interest::READ, Mode::Level);
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

        let window_map: Rc<RefCell<WindowMap>> = Default::default();
        let output_map = Rc::new(RefCell::new(OutputMap::new(
            display.clone(),
            window_map.clone(),
            log.clone(),
        )));

        shm::init_shm_global(&mut display.borrow_mut(), vec![], log.clone());

        ShellHandles::init::<B>(display.clone(), log.clone());

        let display_rc = display;
        let mut display_ref = display_rc.borrow_mut();
        let display = &mut *display_ref;

        init_xdg_output_manager(display, log.clone());
        init_xdg_activation_global(display, xdg_activation_global_impl::<B>, log.clone());
        init_xdg_decoration_manager(display, xdg_decoration_impl, log.clone());

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
                DataDeviceEvent::DnDDropped { .. } => drag_icon.set(None),
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

        drop(display_ref);
        let display = display_rc;

        let xwayland = {
            let (xwayland, channel) = XWayland::new(handle.clone(), display.clone(), log.clone());
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
            running: true.into(),
            display,
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

fn xdg_activation_global_impl<B: 'static>(
    state: &Mutex<XdgActivationState>,
    req: XdgActivationEvent,
    mut ddata: DispatchData<'_>,
) {
    let wm_state = ddata.get::<State<B>>().unwrap();
    match req {
        XdgActivationEvent::RequestActivation {
            token,
            token_data,
            surface,
        } => {
            if token_data.timestamp.elapsed().as_secs() < 10 {
                wm_state
                    .window_map
                    .borrow_mut()
                    .bring_surface_to_top(&surface);
            } else {
                state.lock().unwrap().remove_request(&token);
            }
        }
        XdgActivationEvent::DestroyActivationRequest { .. } => {}
    }
}

fn xdg_decoration_impl(req: XdgDecorationRequest, _ddata: DispatchData<'_>) {
    match req {
        XdgDecorationRequest::NewToplevelDecoration { toplevel } => {
            use xdg_decoration::v1::server::zxdg_toplevel_decoration_v1::Mode;

            if toplevel
                .with_pending_state(|state| state.decoration_mode = Some(Mode::ClientSide))
                .is_ok()
            {
                toplevel.send_configure()
            }
        }
        XdgDecorationRequest::SetMode { .. } | XdgDecorationRequest::UnsetMode { .. } => (),
    }
}
