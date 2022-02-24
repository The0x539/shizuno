use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use smithay::backend::renderer::buffer_dimensions;
use smithay::reexports::*;
use smithay::utils::{Logical, Physical, Point, Rectangle, Size};
use smithay::wayland::{
    compositor::{
        compositor_init, is_sync_subsurface, with_states, BufferAssignment, SurfaceAttributes,
    },
    seat::{AxisFrame, PointerGrab, PointerGrabStartData, PointerInnerHandle, Seat},
    shell::{
        legacy::{wl_shell_init, ShellRequest, ShellState as WlShellState, ShellSurfaceKind},
        wlr_layer::{wlr_layer_shell_init, LayerShellRequest, LayerSurfaceAttributes},
        xdg::{
            xdg_shell_init, Configure, ShellState as XdgShellState, SurfaceCachedState,
            XdgPopupSurfaceRoleAttributes, XdgRequest, XdgToplevelSurfaceRoleAttributes,
        },
    },
    Serial,
};

use bitflags::bitflags;
use slog::Logger;
use wayland_protocols::xdg_shell::server::xdg_toplevel;
use wayland_server::{
    protocol::{
        wl_buffer::WlBuffer, wl_output::WlOutput, wl_pointer::ButtonState, wl_shell_surface,
        wl_surface::WlSurface,
    },
    DispatchData, Display,
};

use crate::util::with_surface_tree_upward_all;
use crate::window_map::PopupKind;
use crate::{
    output_map::OutputMap,
    state::State,
    window_map::{Kind as SurfaceKind, SurfaceTrait, WindowMap},
};

struct MoveSurfaceGrab {
    start_data: PointerGrabStartData,
    window_map: Rc<RefCell<WindowMap>>,
    toplevel: SurfaceKind,
    initial_window_location: Point<i32, Logical>,
}

impl PointerGrab for MoveSurfaceGrab {
    fn motion(
        &mut self,
        _handle: &mut PointerInnerHandle<'_>,
        location: Point<f64, Logical>,
        _focus: Option<(WlSurface, Point<i32, Logical>)>,
        _serial: Serial,
        _time: u32,
    ) {
        let delta = location - self.start_data.location;
        let new_location = self.initial_window_location.to_f64() + delta;
        self.window_map.borrow_mut().set_location(
            &self.toplevel,
            (new_location.x as i32, new_location.y as i32).into(),
        );
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
    toplevel: SurfaceKind,
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
        // It is impossible to get `min_size` and `max_size` of dead toplevel, so we return early.
        if !self.toplevel.alive() | self.toplevel.get_surface().is_none() {
            handle.unset_grab(serial, time);
            return;
        }

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

        let (min_size, max_size) = with_states(self.toplevel.get_surface().unwrap(), |states| {
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

        match &self.toplevel {
            SurfaceKind::Xdg(xdg) => {
                let ret = xdg.with_pending_state(|state| {
                    state.states.set(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                if ret.is_ok() {
                    xdg.send_configure();
                }
            }
            SurfaceKind::Wl(wl) => wl.send_configure(self.last_window_size, self.edges.into()),
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

            // If toplevel is dead, we can't resize it, so we return early.
            if !self.toplevel.alive() | self.toplevel.get_surface().is_none() {
                return;
            }

            if let SurfaceKind::Xdg(xdg) = &self.toplevel {
                let ret = xdg.with_pending_state(|state| {
                    state.states.unset(xdg_toplevel::State::Resizing);
                    state.size = Some(self.last_window_size);
                });
                if ret.is_ok() {
                    xdg.send_configure();
                }

                with_states(self.toplevel.get_surface().unwrap(), |states| {
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
                with_states(self.toplevel.get_surface().unwrap(), |states| {
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
    pub wl_state: Arc<Mutex<WlShellState>>,
}

fn fullscreen_output_geometry(
    wl_surface: &WlSurface,
    wl_output: Option<&WlOutput>,
    window_map: &WindowMap,
    output_map: &OutputMap,
) -> Option<Rectangle<i32, Logical>> {
    if let Some(wl_output) = wl_output {
        return output_map.find_by_output(wl_output).map(|o| o.geometry());
    }

    if let Some(location) = window_map
        .find(wl_surface)
        .and_then(|kind| window_map.location(&kind))
    {
        if let Some(result) = output_map.find_by_position(location).map(|o| o.geometry()) {
            return Some(result);
        }
    }

    output_map.with_primary().map(|o| o.geometry())
}

impl ShellHandles {
    pub fn init<B: 'static>(display: Rc<RefCell<Display>>, log: Logger) -> Self {
        let display = &mut display.borrow_mut();
        compositor_init(
            display,
            move |surface, mut ddata| {
                let state = ddata.get::<State<B>>().unwrap();
                surface_commit(&surface, &state.window_map, &state.output_map);
            },
            log.clone(),
        );

        let (xdg_state, _) = xdg_shell_init(display, xdg_shell_impl::<B>, log.clone());
        let (wl_state, _) = wl_shell_init(display, wl_shell_impl::<B>, log.clone());
        wlr_layer_shell_init(display, wlr_layer_shell_impl::<B>, log.clone());

        Self {
            xdg_state,
            wl_state,
        }
    }
}

#[derive(Default)]
pub struct SurfaceData {
    pub buffer: Option<WlBuffer>,
    pub texture: Option<Box<dyn Any + 'static>>,
    resize_state: ResizeState,
    buffer_dimensions: Option<Size<i32, Physical>>,
    pub buffer_scale: i32,
}

impl SurfaceData {
    fn update_buffer(&mut self, attrs: &mut SurfaceAttributes) {
        match attrs.buffer.take() {
            Some(BufferAssignment::NewBuffer { buffer, .. }) => {
                self.buffer_dimensions = buffer_dimensions(&buffer);
                self.buffer_scale = attrs.buffer_scale;
                if let Some(old_buffer) = self.buffer.replace(buffer) {
                    old_buffer.release();
                }
                self.texture = None;
            }
            Some(BufferAssignment::Removed) => {
                self.buffer = None;
                self.buffer_dimensions = None;
                self.texture = None;
            }
            None => (),
        }
    }

    pub fn size(&self) -> Option<Size<i32, Logical>> {
        let dims = self.buffer_dimensions?;
        Some(dims.to_logical(self.buffer_scale))
    }

    pub fn contains_point(&self, attrs: &SurfaceAttributes, point: Point<f64, Logical>) -> bool {
        let size = try_or!(return false, self.size());
        let rect = Rectangle::from_loc_and_size((0, 0), size).to_f64();
        if !rect.contains(point) {
            return false;
        }
        // if there's no input region, then the entire window works
        let input_region = try_or!(return true, attrs.input_region.as_ref());
        input_region.contains(point.to_i32_floor())
    }

    pub fn send_frame(attrs: &mut SurfaceAttributes, time: Duration) {
        for callback in attrs.frame_callbacks.drain(..) {
            callback.done(time.as_millis() as u32);
        }
    }
}

fn xdg_shell_impl<B: 'static>(shell_event: XdgRequest, mut ddata: DispatchData<'_>) {
    let state = ddata.get::<State<B>>().unwrap();
    match shell_event {
        XdgRequest::NewToplevel { surface } => {
            use rand::distributions::{Distribution, Uniform};

            let geometry = match state.output_map.borrow().with_primary() {
                Some(o) => o.geometry(),
                None => Rectangle::from_loc_and_size((0, 0), (800, 800)),
            };

            let max_x = geometry.loc.x + (((geometry.size.w as f32) / 3.0) * 2.0) as i32;
            let max_y = geometry.loc.y + (((geometry.size.h as f32) / 3.0) * 2.0) as i32;
            let x_range = Uniform::new(geometry.loc.x, max_x);
            let y_range = Uniform::new(geometry.loc.y, max_y);
            let mut rng = rand::thread_rng();
            let x = x_range.sample(&mut rng);
            let y = y_range.sample(&mut rng);
            state
                .window_map
                .borrow_mut()
                .insert(surface.into(), (x, y).into());
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
            state.window_map.borrow_mut().insert_popup(surface.into());
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

            let toplevel = surface.clone().into();
            let mut initial_window_location =
                state.window_map.borrow().location(&toplevel).unwrap();

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
                window_map: state.window_map.clone(),
                toplevel,
                initial_window_location,
            };

            pointer.set_grab(grab, serial);
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

            let toplevel = surface.clone().into();
            let initial_window_location = state.window_map.borrow().location(&toplevel).unwrap();
            let geometry = state.window_map.borrow().geometry(&toplevel).unwrap();
            let initial_window_size = geometry.size;

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
                toplevel,
                edges,
                initial_window_size,
                last_window_size: initial_window_size,
            };

            pointer.set_grab(grab, serial);
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
            surface, output, ..
        } => {
            let wl_surface = try_or!(return, surface.get_surface());

            let geometry = try_or!(
                return,
                fullscreen_output_geometry(
                    wl_surface,
                    output.as_ref(),
                    &state.window_map.borrow(),
                    &state.output_map.borrow(),
                ),
            );

            let mut window_map = state.window_map.borrow_mut();
            if let Some(kind) = window_map.find(wl_surface) {
                window_map.set_location(&kind, geometry.loc);
            }

            let ret = surface.with_pending_state(|state| {
                state.states.set(xdg_toplevel::State::Fullscreen);
                state.size = Some(geometry.size);
                state.fullscreen_output = output;
            });
            if ret.is_ok() {
                surface.send_configure();
            }
        }

        XdgRequest::UnFullscreen { surface } => {
            let ret = surface.with_pending_state(|state| {
                state.states.unset(xdg_toplevel::State::Fullscreen);
                state.size = None;
                state.fullscreen_output = None;
            });
            if ret.is_ok() {
                surface.send_configure();
            }
        }

        XdgRequest::Maximize { surface } => {
            // NOTE: This should use layer-shell when it is implemented
            // to get the correct maximum size

            let mut window_map = state.window_map.borrow_mut();

            let wl_surface = try_or!(return, surface.get_surface());
            let kind = try_or!(return, window_map.find(wl_surface));

            let geometry = {
                let output_map = state.output_map.borrow();

                let position = try_or!(return, window_map.location(&kind));
                let output = try_or!(return, output_map.find_by_position(position));
                output.geometry()
            };

            if let Some(kind) = window_map.find(wl_surface) {
                window_map.set_location(&kind, geometry.loc);
            }

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
        _ => (),
    }
}

fn wl_shell_impl<B: 'static>(req: ShellRequest, mut ddata: DispatchData<'_>) {
    let state = ddata.get::<State<B>>().unwrap();
    match req {
        ShellRequest::SetKind {
            surface,
            kind: ShellSurfaceKind::Toplevel,
        } => {
            // place the window at a random location on the primary output
            // or if there is not output in a [0;800]x[0;800] square
            use rand::distributions::{Distribution, Uniform};

            let geometry = match state.output_map.borrow().with_primary() {
                Some(o) => o.geometry(),
                None => Rectangle::from_loc_and_size((0, 0), (800, 800)),
            };

            let max_x = geometry.loc.x + (((geometry.size.w as f32) / 3.0) * 2.0) as i32;
            let max_y = geometry.loc.y + (((geometry.size.h as f32) / 3.0) * 2.0) as i32;
            let x_range = Uniform::new(geometry.loc.x, max_x);
            let y_range = Uniform::new(geometry.loc.y, max_y);
            let mut rng = rand::thread_rng();
            let x = x_range.sample(&mut rng);
            let y = y_range.sample(&mut rng);
            state
                .window_map
                .borrow_mut()
                .insert(SurfaceKind::Wl(surface), (x, y).into());
        }
        ShellRequest::SetKind {
            surface,
            kind: ShellSurfaceKind::Fullscreen { output, .. },
        } => {
            // NOTE: This is only one part of the solution. We can set the
            // location and configure size here, but the surface should be rendered fullscreen
            // independently from its buffer size
            let wl_surface = try_or!(return, surface.get_surface());

            let geometry = fullscreen_output_geometry(
                wl_surface,
                output.as_ref(),
                &state.window_map.borrow(),
                &state.output_map.borrow(),
            );

            if let Some(geometry) = geometry {
                state
                    .window_map
                    .borrow_mut()
                    .insert(surface.into(), geometry.loc);
            }
        }
        ShellRequest::Move {
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

            let toplevel = surface.into();
            let initial_window_location = state.window_map.borrow().location(&toplevel).unwrap();

            let grab = MoveSurfaceGrab {
                start_data,
                window_map: state.window_map.clone(),
                toplevel,
                initial_window_location,
            };

            pointer.set_grab(grab, serial);
        }
        ShellRequest::Resize {
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

            let toplevel = surface.clone().into();
            let initial_window_location = state.window_map.borrow().location(&toplevel).unwrap();
            let geometry = state.window_map.borrow().geometry(&toplevel).unwrap();
            let initial_window_size = geometry.size;
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
                toplevel,
                edges,
                initial_window_size,
                last_window_size: initial_window_size,
            };

            pointer.set_grab(grab, serial);
        }
        _ => (),
    }
}

fn wlr_layer_shell_impl<B: 'static>(event: LayerShellRequest, mut ddata: DispatchData<'_>) {
    match event {
        LayerShellRequest::NewLayerSurface {
            surface,
            output,
            layer,
            namespace: _,
        } => {
            let state = ddata.get::<State<B>>().unwrap();
            let output_map = state.output_map.borrow();

            let output = output
                .and_then(|o| output_map.find_by_output(&o))
                .or_else(|| output_map.find_by_position(state.pointer_location.to_i32_round()))
                .or_else(|| output_map.with_primary())
                .unwrap();

            let wl_surface = try_or!(return, surface.get_surface());
            output.add_layer_surface(wl_surface.clone());
            state.window_map.borrow_mut().layers.insert(surface, layer);
        }
        LayerShellRequest::AckConfigure { .. } => (),
    }
}

fn surface_commit(
    surface: &WlSurface,
    window_map: &RefCell<WindowMap>,
    output_map: &RefCell<OutputMap>,
) {
    super::xwayland::commit_hook(surface);
    let mut window_map = window_map.borrow_mut();

    if !is_sync_subsurface(surface) {
        with_surface_tree_upward_all(surface, |_, states| {
            states
                .data_map
                .insert_if_missing::<RefCell<SurfaceData>, _>(Default::default);
            let mut data = states
                .data_map
                .get::<RefCell<SurfaceData>>()
                .unwrap()
                .borrow_mut();
            data.update_buffer(&mut states.cached_state.current::<SurfaceAttributes>());
        });
    }

    macro_rules! ics {
        ($t:ty) => {
            with_states(surface, |states| {
                let attrs = states.data_map.get::<Mutex<$t>>().unwrap();
                attrs.lock().unwrap().initial_configure_sent
            })
            .unwrap()
        };
    }

    if let Some(toplevel) = window_map.find(surface) {
        if let SurfaceKind::Xdg(toplevel) = &toplevel {
            if !ics!(XdgToplevelSurfaceRoleAttributes) {
                toplevel.send_configure();
            }
        }

        window_map.refresh_toplevel(&toplevel);

        let geometry = window_map.geometry(&toplevel).unwrap();
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
                        let mut location = window_map.location(&toplevel).unwrap();

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
            window_map.set_location(&toplevel, location);
        }
    }

    if let Some(popup) = window_map.find_popup(surface) {
        let PopupKind::Xdg(popup) = popup;
        if !ics!(XdgPopupSurfaceRoleAttributes) {
            popup.send_configure().unwrap();
        }
    }

    if let Some(layer) = window_map.layers.find(surface) {
        if !ics!(LayerSurfaceAttributes) {
            layer.surface.send_configure();
        }

        if let Some(output) = output_map.borrow().find_by_layer_surface(surface) {
            window_map.layers.arrange_layers(output);
        }
    }
}
