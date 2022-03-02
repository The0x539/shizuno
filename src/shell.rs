use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use smithay::backend::renderer::utils::on_commit_buffer_handler;
use smithay::desktop::{
    layer_map_for_output, Kind as SurfaceKind, LayerSurface, PopupKeyboardGrab, PopupKind,
    PopupManager, PopupPointerGrab, PopupUngrabStrategy, Space, Window,
};
use smithay::reexports::*;
use smithay::utils::{Buffer, Logical, Point, Rectangle, Size};
use smithay::wayland::{
    compositor::{compositor_init, with_states},
    output::Output,
    seat::{AxisFrame, PointerGrab, PointerGrabStartData, PointerInnerHandle, Seat},
    shell::{
        wlr_layer::{
            wlr_layer_shell_init, LayerShellRequest, LayerShellState, LayerSurfaceAttributes,
        },
        xdg::{
            xdg_shell_init, Configure, ShellState as XdgShellState, SurfaceCachedState,
            XdgPopupSurfaceRoleAttributes, XdgRequest, XdgToplevelSurfaceRoleAttributes,
        },
    },
    Serial,
};

use bitflags::bitflags;
use slog::{trace, warn, Logger};
use wayland_protocols::xdg_shell::server::xdg_toplevel;
use wayland_server::{
    protocol::{
        wl_output::WlOutput, wl_pointer::ButtonState, wl_shell_surface, wl_surface::WlSurface,
    },
    DispatchData, Display,
};

use crate::state::Backend;
use crate::{
    state::State,
    util::{with_surface_tree_upward_all, PseudoCell},
};

struct MoveSurfaceGrab {
    start_data: PointerGrabStartData,
    space: Rc<RefCell<Space>>,
    window: Window,
    initial_window_location: Point<i32, Logical>,
}

impl PointerGrab for MoveSurfaceGrab {
    fn motion(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        location: Point<f64, Logical>,
        _focus: Option<(WlSurface, Point<i32, Logical>)>,
        serial: Serial,
        time: u32,
    ) {
        handle.motion(location, None, serial, time);

        let delta = location - self.start_data.location;
        let new_location = self.initial_window_location.to_f64() + delta;

        self.space
            .borrow_mut()
            .map_window(&self.window, new_location.to_i32_round(), true);
    }

    fn button(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        button: u32,
        state: ButtonState,
        serial: Serial,
        time: u32,
    ) {
        handle.button(button, state, serial, time);
        if handle.current_pressed().is_empty() {
            handle.unset_grab(serial, time);
        }
    }

    fn axis(&mut self, handle: &mut PointerInnerHandle<'_>, details: AxisFrame) {
        handle.axis(details)
    }

    fn start_data(&self) -> &PointerGrabStartData {
        &self.start_data
    }
}

struct ResizeSurfaceGrab {
    start_data: PointerGrabStartData,
    window: Window,
    edges: ResizeEdge,
    initial_window_size: Size<i32, Logical>,
    last_window_size: Size<i32, Logical>,
}

impl PointerGrab for ResizeSurfaceGrab {
    fn motion(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        location: Point<f64, Logical>,
        _focus: Option<(WlSurface, Point<i32, Logical>)>,
        serial: Serial,
        time: u32,
    ) {
        let toplevel = self.window.toplevel();

        // It is impossible to get `min_size` and `max_size` of dead toplevel, so we return early.
        if !toplevel.alive() || toplevel.get_surface().is_none() {
            handle.unset_grab(serial, time);
            return;
        }

        handle.motion(location, None, serial, time);

        let (mut dx, mut dy) = (location - self.start_data.location).into();

        let mut new_window_width = self.initial_window_size.w;
        let mut new_window_height = self.initial_window_size.h;

        let left_right = ResizeEdge::LEFT | ResizeEdge::RIGHT;
        let top_bottom = ResizeEdge::TOP | ResizeEdge::BOTTOM;

        if self.edges.intersects(left_right) {
            if self.edges.intersects(ResizeEdge::LEFT) {
                dx = -dx;
            }

            new_window_width = (self.initial_window_size.w as f64 + dx) as i32;
        }

        if self.edges.intersects(top_bottom) {
            if self.edges.intersects(ResizeEdge::TOP) {
                dy = -dy;
            }

            new_window_height = (self.initial_window_size.h as f64 + dy) as i32;
        }

        let (min_size, max_size) = with_states(toplevel.get_surface().unwrap(), |states| {
            let data = states.cached_state.current::<SurfaceCachedState>();
            (data.min_size, data.max_size)
        })
        .unwrap();

        let min_width = min_size.w.max(1);
        let min_height = min_size.h.max(1);
        let max_width = if max_size.w == 0 {
            i32::max_value()
        } else {
            max_size.w
        };
        let max_height = if max_size.h == 0 {
            i32::max_value()
        } else {
            max_size.h
        };

        new_window_width = new_window_width.max(min_width).min(max_width);
        new_window_height = new_window_height.max(min_height).min(max_height);

        self.last_window_size = (new_window_width, new_window_height).into();

        match toplevel {
            SurfaceKind::Xdg(xdg) => {
                let ret = xdg.with_pending_state(|state| {
                    state.states.set(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                if ret.is_ok() {
                    xdg.send_configure();
                }
            }
            SurfaceKind::X11(_) => {
                // TODO: What to do here? Send the update via X11?
            }
        }
    }

    fn button(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        button: u32,
        state: ButtonState,
        serial: Serial,
        time: u32,
    ) {
        handle.button(button, state, serial, time);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(serial, time);

            let toplevel = self.window.toplevel();

            // If toplevel is dead, we can't resize it, so we return early.
            if !toplevel.alive() || toplevel.get_surface().is_none() {
                return;
            }

            if let SurfaceKind::Xdg(xdg) = toplevel {
                let ret = xdg.with_pending_state(|state| {
                    state.states.unset(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                if ret.is_ok() {
                    xdg.send_configure();
                }

                with_states(toplevel.get_surface().unwrap(), |states| {
                    let mut data = states
                        .data_map
                        .get::<RefCell<SurfaceData>>()
                        .unwrap()
                        .borrow_mut();
                    if let ResizeState::Resizing(resize_data) = data.resize_state {
                        data.resize_state = ResizeState::WaitingForFinalAck(resize_data, serial);
                    } else {
                        panic!("invalid resize state: {:?}", data.resize_state);
                    }
                })
                .unwrap();
            } else {
                with_states(toplevel.get_surface().unwrap(), |states| {
                    let mut data = states
                        .data_map
                        .get::<RefCell<SurfaceData>>()
                        .unwrap()
                        .borrow_mut();
                    if let ResizeState::Resizing(resize_data) = data.resize_state {
                        data.resize_state = ResizeState::WaitingForCommit(resize_data);
                    } else {
                        panic!("invalid resize state: {:?}", data.resize_state);
                    }
                })
                .unwrap();
            }
        }
    }

    fn axis(&mut self, handle: &mut PointerInnerHandle<'_>, details: AxisFrame) {
        handle.axis(details)
    }

    fn start_data(&self) -> &PointerGrabStartData {
        &self.start_data
    }
}

#[derive(Debug, Copy, Clone)]
struct ResizeData {
    edges: ResizeEdge,
    initial_window_location: Point<i32, Logical>,
    initial_window_size: Size<i32, Logical>,
}

#[derive(Debug, Copy, Clone)]
enum ResizeState {
    NotResizing,
    Resizing(ResizeData),
    WaitingForFinalAck(ResizeData, Serial),
    WaitingForCommit(ResizeData),
}

impl Default for ResizeState {
    fn default() -> Self {
        Self::NotResizing
    }
}

bitflags! {
    struct ResizeEdge: u32 {
        const NONE = 0;
        const TOP = 1;
        const BOTTOM = 2;
        const LEFT = 4;
        const TOP_LEFT = 5;
        const BOTTOM_LEFT = 6;
        const RIGHT = 8;
        const TOP_RIGHT = 9;
        const BOTTOM_RIGHT = 10;
    }
}

impl From<ResizeEdge> for wl_shell_surface::Resize {
    fn from(x: ResizeEdge) -> Self {
        Self::from_bits(x.bits()).unwrap()
    }
}

impl From<wl_shell_surface::Resize> for ResizeEdge {
    fn from(x: wl_shell_surface::Resize) -> Self {
        Self::from_bits(x.bits()).unwrap()
    }
}

impl From<xdg_toplevel::ResizeEdge> for ResizeEdge {
    fn from(x: xdg_toplevel::ResizeEdge) -> Self {
        Self::from_bits(x.to_raw()).unwrap()
    }
}

impl From<ResizeEdge> for xdg_toplevel::ResizeEdge {
    fn from(x: ResizeEdge) -> Self {
        Self::from_raw(x.bits()).unwrap()
    }
}

pub struct ShellHandles {
    pub xdg_state: Arc<Mutex<XdgShellState>>,
    //pub wl_state: Arc<Mutex<WlShellState>>,
    pub layer_state: Arc<Mutex<LayerShellState>>,
}

fn fullscreen_output_geometry(
    wl_surface: &WlSurface,
    wl_output: Option<&WlOutput>,
    space: &mut Space,
) -> Option<Rectangle<i32, Logical>> {
    let output = if let Some(output) = wl_output.and_then(Output::from_resource) {
        output
    } else {
        let window = space.window_for_surface(wl_surface)?;
        space.outputs_for_window(window).first()?.clone()
    };
    space.output_geometry(&output)
}

#[derive(Default)]
pub struct FullscreenSurface(RefCell<Option<Window>>);

impl FullscreenSurface {
    pub fn set(&self, window: Window) {
        self.0.set(Some(window));
    }

    pub fn get(&self) -> Option<Window> {
        self.0.borrow().clone()
    }

    pub fn clear(&self) -> Option<Window> {
        self.0.borrow_mut().take()
    }
}

impl ShellHandles {
    pub fn init<B: Backend + 'static>(display: Rc<RefCell<Display>>, log: Logger) -> Self {
        let display = &mut display.borrow_mut();
        compositor_init(
            display,
            move |surface, mut ddata| {
                on_commit_buffer_handler(&surface);
                let state = ddata.get::<State<B>>().unwrap();
                let mut popups = state.popups.borrow_mut();
                let space = state.space.as_ref();
                space.borrow_mut().commit(&surface);
                surface_commit(&surface, space, &mut popups);
            },
            log.clone(),
        );

        let l = log.clone();
        let (xdg_state, _) = xdg_shell_init(
            display,
            move |s, d| xdg_shell_impl::<B>(s, d, &l),
            log.clone(),
        );

        let (layer_state, _) =
            wlr_layer_shell_init(display, wlr_layer_shell_impl::<B>, log.clone());

        Self {
            xdg_state,
            layer_state,
        }
    }
}

#[derive(Default)]
pub struct SurfaceData {
    resize_state: ResizeState,
    pub buffer_dimensions: Option<Size<i32, Buffer>>,
}

fn xdg_shell_impl<B: Backend + 'static>(
    shell_event: XdgRequest,
    mut ddata: DispatchData<'_>,
    log: &Logger,
) {
    let state = ddata.get::<State<B>>().unwrap();
    match shell_event {
        XdgRequest::NewToplevel { surface } => {
            let window = Window::new(SurfaceKind::Xdg(surface));
            place_new_window(&mut state.space.borrow_mut(), &window, true);
        }

        XdgRequest::NewPopup {
            surface,
            positioner,
        } => {
            // TODO: properly recompute geometry
            // TODO: "this is not really necessary"
            surface
                .with_pending_state(|state| state.geometry = positioner.get_geometry())
                .unwrap();

            if let Err(e) = state.popups.borrow_mut().track_popup(surface.into()) {
                warn!(log, "Failed to track popup: {e}");
            }
        }

        XdgRequest::RePosition {
            surface,
            positioner,
            token,
        } => {
            // TODO: calculate real popup geometry
            let res = surface.with_pending_state(|state| {
                state.geometry = positioner.get_geometry();
                state.positioner = positioner;
            });
            if res.is_ok() {
                surface.send_repositioned(token);
            }
        }

        XdgRequest::Move {
            surface,
            seat,
            serial,
        } => {
            let seat = Seat::from_resource(&seat).unwrap();
            // TODO: touch move
            let pointer = seat.get_pointer().unwrap();

            if !pointer.has_grab(serial) {
                return;
            }

            let start_data = pointer.grab_start_data().unwrap();

            let (focus, _) = try_or!(return, start_data.focus.as_ref());
            if !focus
                .as_ref()
                .same_client_as(surface.get_surface().unwrap().as_ref())
            {
                return;
            }

            let space = state.space.clone();
            let window = space
                .borrow_mut()
                .window_for_surface(surface.get_surface().unwrap())
                .unwrap()
                .clone();
            let mut initial_window_location = space.borrow().window_location(&window).unwrap();

            if let Some(cur_state) = surface.current_state() {
                if cur_state.states.contains(xdg_toplevel::State::Maximized) {
                    let fs_changed = surface.with_pending_state(|state| {
                        state.states.unset(xdg_toplevel::State::Maximized);
                        state.size = None;
                    });

                    if fs_changed.is_ok() {
                        surface.send_configure();
                        // NOTE: mouse location should be mapped to a new window size
                        // 1) transform mouse pointer position from compositor space to window space (location relative)
                        // 2) divide the x coordinate by width of the window to get the percentage
                        //   - 0.0 would be on the far left of the window
                        //   - 0.5 would be in middle of the window
                        //   - 1.0 would be on the far right of the window
                        // 3) multiply the percentage by new window width
                        // 4) by doing that, drag will look a lot more natural
                        initial_window_location = pointer.current_location().to_i32_round();
                    }
                }
            }

            let grab = MoveSurfaceGrab {
                start_data,
                space,
                window,
                initial_window_location,
            };

            pointer.set_grab(grab, serial, 0);
        }

        XdgRequest::Resize {
            surface,
            seat,
            serial,
            edges,
        } => {
            let seat = Seat::from_resource(&seat).unwrap();
            // TODO: touch resize
            let pointer = seat.get_pointer().unwrap();

            if !pointer.has_grab(serial) {
                return;
            }

            let start_data = pointer.grab_start_data().unwrap();

            let (focus, _) = try_or!(return, start_data.focus.as_ref());
            if !focus
                .as_ref()
                .same_client_as(surface.get_surface().unwrap().as_ref())
            {
                return;
            }

            let space = state.space.clone();
            let window = space
                .borrow_mut()
                .window_for_surface(surface.get_surface().unwrap())
                .unwrap()
                .clone();

            let geometry = window.geometry();
            let loc = space.borrow().window_location(&window).unwrap();
            let (initial_window_location, initial_window_size) = (loc, geometry.size);

            let edges = edges.into();
            with_states(surface.get_surface().unwrap(), move |states| {
                let data = states.data_map.get::<RefCell<SurfaceData>>().unwrap();
                data.borrow_mut().resize_state = ResizeState::Resizing(ResizeData {
                    edges,
                    initial_window_location,
                    initial_window_size,
                });
            })
            .unwrap();

            let grab = ResizeSurfaceGrab {
                start_data,
                window,
                edges,
                initial_window_size,
                last_window_size: initial_window_size,
            };

            pointer.set_grab(grab, serial, 0);
        }

        XdgRequest::AckConfigure {
            surface,
            configure: Configure::Toplevel(configure),
            ..
        } => {
            let waiting_for_serial = with_states(&surface, |states| {
                if let Some(data) = states.data_map.get::<RefCell<SurfaceData>>() {
                    if let ResizeState::WaitingForFinalAck(_, serial) = data.borrow().resize_state {
                        return Some(serial);
                    }
                }

                None
            })
            .unwrap();

            let serial = try_or!(return, waiting_for_serial);

            let is_resizing = with_states(&surface, |states| {
                states
                    .data_map
                    .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .current
                    .states
                    .contains(xdg_toplevel::State::Resizing)
            })
            .unwrap();

            if configure.serial >= serial && is_resizing {
                with_states(&surface, |states| {
                    let mut data = states
                        .data_map
                        .get::<RefCell<SurfaceData>>()
                        .unwrap()
                        .borrow_mut();
                    if let ResizeState::WaitingForFinalAck(resize_data, _) = data.resize_state {
                        data.resize_state = ResizeState::WaitingForCommit(resize_data);
                    } else {
                        unreachable!()
                    }
                })
                .unwrap();
            }
        }

        XdgRequest::Fullscreen {
            surface,
            output: mut wl_output,
            ..
        } => {
            let wl_surface = try_or!(return, surface.get_surface());

            let geometry = try_or!(
                return,
                fullscreen_output_geometry(
                    wl_surface,
                    wl_output.as_ref(),
                    &mut state.space.borrow_mut(),
                ),
            );

            let space = state.space.borrow_mut();
            let output = wl_output
                .as_ref()
                .and_then(Output::from_resource)
                .unwrap_or_else(|| space.outputs().next().unwrap().clone());

            output.with_client_outputs(wl_surface.as_ref().client().unwrap(), |o| {
                wl_output = Some(o.clone())
            });

            let ret = surface.with_pending_state(|state| {
                state.states.set(xdg_toplevel::State::Fullscreen);
                state.size = Some(geometry.size);
                state.fullscreen_output = wl_output;
            });
            if ret.is_ok() {
                let window = space.window_for_surface(wl_surface).unwrap();
                window.configure();
                output
                    .user_data()
                    .insert_if_missing(FullscreenSurface::default);
                output
                    .user_data()
                    .get::<FullscreenSurface>()
                    .unwrap()
                    .set(window.clone());
                trace!(log, "Fullscreening: {window:?}");
            }
        }

        XdgRequest::UnFullscreen { surface } => {
            let ret = surface.with_pending_state(|state| {
                state.states.unset(xdg_toplevel::State::Fullscreen);
                state.size = None;
                state.fullscreen_output.take()
            });
            if let Ok(output) = ret {
                if let Some(output) = output {
                    let output = Output::from_resource(&output).unwrap();
                    if let Some(fullscreen) = output.user_data().get::<FullscreenSurface>() {
                        trace!(log, "Unfullscreening: {:?}", fullscreen.get());
                        fullscreen.clear();
                        state.backend_data.reset_buffers(&output);
                    }
                }
                surface.send_configure();
            }
        }

        XdgRequest::Maximize { surface } => {
            // NOTE: This should use layer-shell when it is implemented
            // to get the correct maximum size

            let mut space = state.space.borrow_mut();
            let window = space
                .window_for_surface(surface.get_surface().unwrap())
                .unwrap()
                .clone();
            let output = &space.outputs_for_window(&window)[0];
            let geometry = space.output_geometry(output).unwrap();

            space.map_window(&window, geometry.loc, true);

            let ret = surface.with_pending_state(|state| {
                state.states.set(xdg_toplevel::State::Maximized);
                state.size = Some(geometry.size);
            });
            if ret.is_ok() {
                surface.send_configure();
            }
        }
        XdgRequest::UnMaximize { surface } => {
            let ret = surface.with_pending_state(|state| {
                state.states.unset(xdg_toplevel::State::Maximized);
                state.size = None;
            });
            if ret.is_ok() {
                surface.send_configure();
            }
        }
        XdgRequest::Grab {
            serial,
            surface,
            seat,
        } => {
            let seat = Seat::from_resource(&seat).unwrap();
            let ret = state
                .popups
                .borrow_mut()
                .grab_popup(surface.into(), &seat, serial);

            let mut grab = try_or!(return, ret.ok());
            let prev = grab.previous_serial().unwrap_or(serial);

            if let Some(keyboard) = seat.get_keyboard() {
                let (is_grabbed, has_grab, had_grab) = (
                    keyboard.is_grabbed(),
                    keyboard.has_grab(serial),
                    keyboard.has_grab(prev),
                );
                if is_grabbed && !(has_grab || had_grab) {
                    grab.ungrab(PopupUngrabStrategy::All);
                    return;
                }
                keyboard.set_focus(grab.current_grab().as_ref(), serial);
                keyboard.set_grab(PopupKeyboardGrab::new(&grab), serial);
            }

            if let Some(pointer) = seat.get_pointer() {
                let (is_grabbed, has_grab, had_grab) = (
                    pointer.is_grabbed(),
                    pointer.has_grab(serial),
                    pointer.has_grab(prev),
                );
                if is_grabbed && !(has_grab || had_grab) {
                    grab.ungrab(PopupUngrabStrategy::All);
                    return;
                }
                pointer.set_grab(PopupPointerGrab::new(&grab), serial, 0);
            }
        }
        _ => (),
    }
}

fn wlr_layer_shell_impl<B: 'static>(event: LayerShellRequest, mut ddata: DispatchData<'_>) {
    match event {
        LayerShellRequest::NewLayerSurface {
            surface,
            output: wl_output,
            namespace,
            ..
        } => {
            let state = ddata.get::<State<B>>().unwrap();
            let space = state.space.borrow();
            let output = wl_output
                .as_ref()
                .and_then(Output::from_resource)
                .or_else(|| space.outputs().next().cloned())
                .unwrap();

            let mut map = layer_map_for_output(&output);
            map.map_layer(&LayerSurface::new(surface, namespace))
                .unwrap();
        }
        LayerShellRequest::AckConfigure { .. } => (),
    }
}

fn surface_commit(surface: &WlSurface, space: &RefCell<Space>, popups: &mut PopupManager) {
    super::xwayland::commit_hook(surface);
    let mut space = space.borrow_mut();

    with_surface_tree_upward_all(surface, |_, states| {
        states
            .data_map
            .insert_if_missing(RefCell::<SurfaceData>::default);
    });

    macro_rules! ics {
        ($t:ty) => {
            with_states(surface, |states| {
                let attrs = states.data_map.get::<Mutex<$t>>().unwrap();
                attrs.lock().unwrap().initial_configure_sent
            })
            .unwrap()
        };
    }

    if let Some(window) = space.window_for_surface(surface) {
        if let SurfaceKind::Xdg(toplevel) = window.toplevel() {
            if !ics!(XdgToplevelSurfaceRoleAttributes) {
                toplevel.send_configure();
            }
        }

        let geometry = window.geometry();
        let window_loc = space.window_location(window).unwrap();
        let new_location = with_states(surface, |states| {
            let mut data = states
                .data_map
                .get::<RefCell<SurfaceData>>()
                .unwrap()
                .borrow_mut();

            let mut new_location = None;

            match data.resize_state {
                ResizeState::Resizing(resize_data)
                | ResizeState::WaitingForFinalAck(resize_data, _)
                | ResizeState::WaitingForCommit(resize_data) => {
                    let ResizeData {
                        edges,
                        initial_window_location,
                        initial_window_size,
                    } = resize_data;

                    if edges.intersects(ResizeEdge::TOP_LEFT) {
                        let mut location = window_loc;

                        if edges.intersects(ResizeEdge::LEFT) {
                            location.x = initial_window_location.x
                                + (initial_window_size.w - geometry.size.w);
                        }
                        if edges.intersects(ResizeEdge::TOP) {
                            location.y = initial_window_location.y
                                + (initial_window_size.h - geometry.size.h);
                        }

                        new_location = Some(location);
                    }
                }
                ResizeState::NotResizing => (),
            }

            if let ResizeState::WaitingForCommit(_) = data.resize_state {
                data.resize_state = ResizeState::NotResizing;
            }

            new_location
        })
        .unwrap();

        if let Some(location) = new_location {
            let window = window.clone();
            space.map_window(&window, location, true);
        }

        return;
    }

    if let Some(popup) = popups.find_popup(surface) {
        let PopupKind::Xdg(popup) = popup;
        if !ics!(XdgPopupSurfaceRoleAttributes) {
            popup.send_configure().unwrap();
        }
        return;
    }

    if let Some(output) = space
        .outputs()
        .find(|o| layer_map_for_output(o).layer_for_surface(surface).is_some())
        .cloned()
    {
        let mut map = layer_map_for_output(&output);
        let layer = map.layer_for_surface(surface).unwrap();

        if !ics!(LayerSurfaceAttributes) {
            layer.layer_surface().send_configure();
        }

        map.arrange();
    };
}

fn place_new_window(space: &mut Space, window: &Window, activate: bool) {
    use rand::distributions::{Distribution, Uniform};

    let geometry = space
        .outputs()
        .next()
        .cloned()
        .and_then(|o| {
            let g = space.output_geometry(&o)?;
            let m = layer_map_for_output(&o);
            let z = m.non_exclusive_zone();
            Some(Rectangle::from_loc_and_size(g.loc + z.loc, z.size))
        })
        .unwrap_or(Rectangle {
            loc: (0, 0).into(),
            size: (800, 800).into(),
        });

    let max_x = geometry.loc.x + (((geometry.size.w as f32) / 3.0) * 2.0) as i32;
    let max_y = geometry.loc.y + (((geometry.size.h as f32) / 3.0) * 2.0) as i32;
    let x_range = Uniform::new(geometry.loc.x, max_x);
    let y_range = Uniform::new(geometry.loc.y, max_y);
    let mut rng = rand::thread_rng();
    let x = x_range.sample(&mut rng);
    let y = y_range.sample(&mut rng);

    space.map_window(window, (x, y), activate);
}

pub fn fixup_positions(space: &mut Space) {
    let mut offset = Point::<i32, Logical>::default();

    let outputs = space.outputs().cloned().collect::<Vec<_>>();
    for output in &outputs {
        let size = space
            .output_geometry(output)
            .map(|g| g.size)
            .unwrap_or_default();
        let scale = space.output_scale(output).unwrap_or_default();
        space.map_output(output, scale, offset);
        layer_map_for_output(output).arrange();
        offset.x += size.w;
    }

    let boxes = outputs
        .iter()
        .flat_map(|o| {
            let g = space.output_geometry(o)?;
            let m = layer_map_for_output(o);
            let z = m.non_exclusive_zone();
            Some(Rectangle::from_loc_and_size(g.loc + z.loc, z.size))
        })
        .collect::<Vec<_>>();

    let mut orphaned_windows = Vec::new();
    for window in space.windows() {
        let window_location = try_or!(continue, space.window_location(window));
        let geo_loc = window.bbox().loc + window_location;
        if !boxes.iter().any(|g| g.contains(geo_loc)) {
            orphaned_windows.push(window.clone());
        }
    }

    for window in orphaned_windows {
        place_new_window(space, &window, false)
    }
}
