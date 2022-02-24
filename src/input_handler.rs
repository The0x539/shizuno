use std::process::Command;

use smithay::desktop::{layer_map_for_output, WindowSurfaceType};
use smithay::reexports::*;
use smithay::utils::{Logical, Point};
use smithay::wayland::shell::wlr_layer::Layer;
use smithay::wayland::{
    compositor::with_states,
    seat::{AxisFrame, FilterResult, KeysymHandle, ModifiersState},
    shell::wlr_layer::LayerSurfaceCachedState,
    tablet_manager::TabletSeatTrait,
    Serial,
};
use smithay::{
    backend::{
        input::{
            self, Device, DeviceCapability, Event, InputBackend, InputEvent, KeyState,
            KeyboardKeyEvent, PointerAxisEvent, PointerButtonEvent, PointerMotionEvent,
            ProximityState, TabletToolButtonEvent, TabletToolEvent, TabletToolProximityEvent,
            TabletToolTipEvent, TabletToolTipState,
        },
        session::Session,
    },
    wayland::shell::wlr_layer::KeyboardInteractivity,
};

use slog::{debug, error, info};
use wayland_server::protocol::{wl_pointer, wl_surface::WlSurface};
use xkbcommon::xkb::Keysym;

use crate::shell::FullscreenSurface;
use crate::{
    state::{Backend, State},
    udev::UdevData,
};

fn scounter() -> Serial {
    smithay::wayland::SERIAL_COUNTER.next_serial()
}

#[derive(PartialEq)]
enum KeyAction {
    Quit,
    VtSwitch(i32),
    Run(String),
    Screen(usize),
    ScaleUp,
    ScaleDown,
    None,
}

impl<B: Backend> State<B> {
    fn keyboard_key_to_action<I: InputBackend>(&mut self, evt: I::KeyboardKeyEvent) -> KeyAction {
        let keycode = evt.key_code();
        let state = evt.state();
        let serial = scounter();
        let time = evt.time();
        debug!(self.log, "key"; "keycode" => keycode, "state" => format!("{state:?}"));

        for layer in self
            .shells
            .layer_state
            .lock()
            .unwrap()
            .layer_surfaces()
            .iter()
            .rev()
        {
            let surface = try_or!(continue, layer.get_surface());
            let data = with_states(surface, |states| {
                *states.cached_state.current::<LayerSurfaceCachedState>()
            })
            .unwrap();
            if data.keyboard_interactivity == KeyboardInteractivity::Exclusive
                && matches!(data.layer, Layer::Top | Layer::Overlay)
            {
                self.keyboard
                    .set_focus(Some(layer.get_surface().unwrap()), serial);
                self.keyboard
                    .input::<(), _>(keycode, state, serial, time, |_, _| FilterResult::Forward);
                return KeyAction::None;
            }
        }

        let filter = |modifiers: &_, handle: KeysymHandle<'_>| {
            let keysym = handle.modified_sym();

            debug!(self.log, "keysym";
                "state" => format!("{state:?}"),
                "mods" => format!("{modifiers:?}"),
                "keysym" => xkbcommon::xkb::keysym_get_name(keysym)
            );

            match state {
                KeyState::Pressed => {
                    if let Some(action) = process_keyboard_shortcut(*modifiers, keysym) {
                        self.suppressed_keys.insert(keysym);
                        return FilterResult::Intercept(action);
                    }
                }

                KeyState::Released => {
                    if self.suppressed_keys.remove(&keysym) {
                        return FilterResult::Intercept(KeyAction::None);
                    }
                }
            }

            FilterResult::Forward
        };

        self.keyboard
            .input(keycode, state, serial, time, filter)
            .unwrap_or(KeyAction::None)
    }

    fn change_scale(&mut self, delta: f64) {
        let mut space = self.space.borrow_mut();

        let pos = self.pointer_location.to_i32_round();
        let output = try_or!(
            return,
            space
                .outputs()
                .find(|o| space.output_geometry(o).unwrap().contains(pos))
                .cloned(),
        );

        let scale = space.output_scale(&output).unwrap();
        let output_location = space.output_geometry(&output).unwrap().loc;

        let new_scale = (scale + delta).max(1.0);
        output.change_current_state(None, None, Some(new_scale.ceil() as i32), None);
        space.map_output(&output, new_scale, output_location);

        let output_location = output_location.to_f64();
        let factor = scale as f64 / new_scale as f64;
        let mut pointer_output_location = self.pointer_location - output_location;
        pointer_output_location.x *= factor;
        pointer_output_location.y *= factor;
        self.pointer_location = output_location + pointer_output_location;
        let ptr_loc = self.pointer_location;

        crate::shell::fixup_positions(&mut space);
        drop(space);
        let under = self.surface_under();
        self.pointer.motion(ptr_loc, under, scounter(), 0);
        self.backend_data.reset_buffers(&output);
    }

    fn on_pointer_button<I: InputBackend>(&mut self, evt: I::PointerButtonEvent) {
        let serial = scounter();

        let state = match evt.state() {
            input::ButtonState::Pressed => wl_pointer::ButtonState::Pressed,
            input::ButtonState::Released => wl_pointer::ButtonState::Released,
        };

        if state == wl_pointer::ButtonState::Pressed {
            self.update_keyboard_focus(serial);
        }

        let button = evt.button_code();
        self.pointer.button(button, state, serial, evt.time());
    }

    fn update_keyboard_focus(&mut self, serial: Serial) {
        if self.pointer.is_grabbed() {
            return;
        }

        let mut space = self.space.borrow_mut();

        if let Some(output) = space.output_under(self.pointer_location).next() {
            let output_geo = space.output_geometry(output).unwrap();
            if let Some(window) = output
                .user_data()
                .get::<FullscreenSurface>()
                .and_then(|f| f.get())
            {
                let surface = window
                    .surface_under(
                        self.pointer_location - output_geo.loc.to_f64(),
                        WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                    )
                    .map(|(s, _)| s);
                self.keyboard.set_focus(surface.as_ref(), serial);
                return;
            }

            let layers = layer_map_for_output(output);
            if let Some(layer) = layers
                .layer_under(Layer::Overlay, self.pointer_location)
                .or_else(|| layers.layer_under(Layer::Top, self.pointer_location))
            {
                if layer.can_receive_keyboard_focus() {
                    let surface = layer
                        .surface_under(
                            self.pointer_location
                                - output_geo.loc.to_f64()
                                - layers.layer_geometry(layer).unwrap().loc.to_f64(),
                            WindowSurfaceType::ALL,
                        )
                        .map(|(s, _)| s);
                    self.keyboard.set_focus(surface.as_ref(), serial);
                    return;
                }
            }
        }

        if let Some(window) = space.window_under(self.pointer_location).cloned() {
            space.raise_window(&window, true);
            let window_loc = space.window_geometry(&window).unwrap().loc;
            let surface = window
                .surface_under(
                    self.pointer_location - window_loc.to_f64(),
                    WindowSurfaceType::TOPLEVEL | WindowSurfaceType::SUBSURFACE,
                )
                .map(|(s, _)| s);
            self.keyboard.set_focus(surface.as_ref(), serial);
            return;
        }

        if let Some(output) = space.output_under(self.pointer_location).next() {
            let output_geo = space.output_geometry(output).unwrap();
            let layers = layer_map_for_output(output);
            if let Some(layer) = layers
                .layer_under(Layer::Bottom, self.pointer_location)
                .or_else(|| layers.layer_under(Layer::Background, self.pointer_location))
            {
                if layer.can_receive_keyboard_focus() {
                    let surface = layer
                        .surface_under(
                            self.pointer_location
                                - output_geo.loc.to_f64()
                                - layers.layer_geometry(layer).unwrap().loc.to_f64(),
                            WindowSurfaceType::ALL,
                        )
                        .map(|(s, _)| s);
                    self.keyboard.set_focus(surface.as_ref(), serial);
                }
            }
        };
    }

    pub fn surface_under(&self) -> Option<(WlSurface, Point<i32, Logical>)> {
        let pos = self.pointer_location;
        let space = self.space.borrow();
        let output = space.outputs().find(|o| {
            let geometry = space.output_geometry(o).unwrap();
            geometry.contains(pos.to_i32_round())
        })?;
        let output_geo = space.output_geometry(output).unwrap();
        let layers = layer_map_for_output(output);

        if let Some(window) = output
            .user_data()
            .get::<FullscreenSurface>()
            .and_then(|f| f.get())
        {
            return window.surface_under(pos - output_geo.loc.to_f64(), WindowSurfaceType::ALL);
        }

        if let Some(layer) = layers
            .layer_under(Layer::Overlay, pos)
            .or_else(|| layers.layer_under(Layer::Top, pos))
        {
            let layer_loc = layers.layer_geometry(layer).unwrap().loc;
            let (s, loc) = layer.surface_under(
                pos - output_geo.loc.to_f64() - layer_loc.to_f64(),
                WindowSurfaceType::ALL,
            )?;
            return Some((s, loc + layer_loc));
        }

        if let Some(window) = space.window_under(pos) {
            let window_loc = space.window_geometry(window).unwrap().loc;
            let (s, loc) =
                window.surface_under(pos - window_loc.to_f64(), WindowSurfaceType::ALL)?;
            return Some((s, loc + window_loc));
        }

        if let Some(layer) = layers
            .layer_under(Layer::Bottom, pos)
            .or_else(|| layers.layer_under(Layer::Background, pos))
        {
            let layer_loc = layers.layer_geometry(layer).unwrap().loc;
            let (s, loc) = layer.surface_under(
                pos - output_geo.loc.to_f64() - layer_loc.to_f64(),
                WindowSurfaceType::ALL,
            )?;
            return Some((s, loc + layer_loc));
        }

        None
    }

    fn on_pointer_axis<I: InputBackend>(&mut self, evt: I::PointerAxisEvent) {
        let source = match evt.source() {
            input::AxisSource::Continuous => wl_pointer::AxisSource::Continuous,
            input::AxisSource::Finger => wl_pointer::AxisSource::Finger,
            input::AxisSource::Wheel | input::AxisSource::WheelTilt => {
                wl_pointer::AxisSource::Wheel
            }
        };

        let horz_disc = evt.amount_discrete(input::Axis::Horizontal);
        let vert_disc = evt.amount_discrete(input::Axis::Vertical);

        let horz = evt
            .amount(input::Axis::Horizontal)
            .unwrap_or_else(|| horz_disc.unwrap() * 3.0);
        let vert = evt
            .amount(input::Axis::Vertical)
            .unwrap_or_else(|| vert_disc.unwrap() * 3.0);

        let mut frame = AxisFrame::new(evt.time()).source(source);

        let mut update = |axis, value, discrete| {
            if value != 0.0 {
                frame = frame.value(axis, value);
                if let Some(d) = discrete {
                    frame = frame.discrete(axis, d as i32);
                }
            } else if source == wl_pointer::AxisSource::Finger {
                frame = frame.stop(axis);
            }
        };

        update(wl_pointer::Axis::HorizontalScroll, horz, horz_disc);
        update(wl_pointer::Axis::VerticalScroll, vert, vert_disc);

        self.pointer.axis(frame);
    }
}

impl State<UdevData> {
    pub fn process_input_event<I: InputBackend>(&mut self, event: InputEvent<I>) {
        match event {
            InputEvent::Keyboard { event } => match self.keyboard_key_to_action::<I>(event) {
                KeyAction::Quit => {
                    info!(self.log, "Quitting.");
                    self.running.set(false);
                }
                KeyAction::VtSwitch(vt) => {
                    info!(self.log, "Trying to switch to vt {vt}");
                    if let Err(e) = self.backend_data.session.change_vt(vt) {
                        error!(self.log, "Error switching to vt {vt}: {e}");
                    }
                }
                KeyAction::Run(cmd) => {
                    info!(self.log, "Starting program"; "cmd" => &cmd);
                    if let Err(e) = Command::new(&cmd).spawn() {
                        error!(
                            self.log,
                            "Failed to start program";
                            "cmd" => cmd,
                            "err" => format!("{e:?}"),
                        );
                    }
                }
                KeyAction::Screen(num) => {
                    let space = self.space.borrow();
                    let output = try_or!(return, space.outputs().nth(num));
                    let geometry = space.output_geometry(output).unwrap();
                    let x = geometry.loc.x as f64 + geometry.size.w as f64 / 2.0;
                    // TODO: assumes horizontal lineup layout
                    let y = geometry.size.h as f64 / 2.0;
                    self.pointer_location = (x, y).into()
                }
                KeyAction::ScaleUp => self.change_scale(0.25),
                KeyAction::ScaleDown => self.change_scale(-0.25),
                KeyAction::None => (),
            },

            InputEvent::PointerMotion { event } => self.on_pointer_move::<I>(event),
            InputEvent::PointerButton { event } => self.on_pointer_button::<I>(event),
            InputEvent::PointerAxis { event } => self.on_pointer_axis::<I>(event),
            InputEvent::TabletToolAxis { event } => self.on_tablet_tool_axis::<I>(event),
            InputEvent::TabletToolProximity { event } => self.on_tablet_tool_proximity::<I>(event),
            InputEvent::TabletToolTip { event } => self.on_tablet_tool_tip::<I>(event),
            InputEvent::TabletToolButton { event } => self.on_tablet_button::<I>(event),

            InputEvent::DeviceAdded { device } => {
                if device.has_capability(DeviceCapability::TabletTool) {
                    self.seat.tablet_seat().add_tablet(&(&device).into());
                }
            }
            InputEvent::DeviceRemoved { device } => {
                if device.has_capability(DeviceCapability::TabletTool) {
                    let tablet_seat = self.seat.tablet_seat();
                    tablet_seat.remove_tablet(&(&device).into());
                    if tablet_seat.count_tablets() == 0 {
                        tablet_seat.clear_tools();
                    }
                }
            }
            _ => (),
        }
    }

    fn on_pointer_move<I: InputBackend>(&mut self, evt: I::PointerMotionEvent) {
        let serial = scounter();

        let ptr_loc = self.clamp_coords(self.pointer_location + evt.delta());
        self.pointer_location = ptr_loc;

        let under = self.surface_under();
        self.pointer.motion(ptr_loc, under, serial, evt.time());
    }

    fn on_tablet_tool_axis<I: InputBackend>(&mut self, evt: I::TabletToolAxisEvent) {
        let tablet_seat = self.seat.tablet_seat();

        let space = self.space.borrow();
        let output = try_or!(return, space.outputs().next());
        let rect = space.output_geometry(output).unwrap();

        self.pointer_location = evt.position_transformed(rect.size) + rect.loc.to_f64();

        let under = self.surface_under();

        let tablet = try_or!(return, tablet_seat.get_tablet(&(&evt.device()).into()));
        let tool = try_or!(return, tablet_seat.get_tool(&evt.tool()));

        macro_rules! field {
            ($field:ident, $has_changed:ident) => {
                if evt.$has_changed() {
                    tool.$field(evt.$field());
                }
            };
        }

        field!(pressure, pressure_has_changed);
        field!(distance, distance_has_changed);
        field!(tilt, tilt_has_changed);
        field!(slider_position, slider_has_changed);
        field!(rotation, rotation_has_changed);
        if evt.wheel_has_changed() {
            tool.wheel(evt.wheel_delta(), evt.wheel_delta_discrete());
        }

        tool.motion(
            self.pointer_location,
            under,
            &tablet,
            scounter(),
            evt.time(),
        );
    }

    fn on_tablet_tool_proximity<I: InputBackend>(&mut self, evt: I::TabletToolProximityEvent) {
        let tablet_seat = self.seat.tablet_seat();
        let space = self.space.borrow();

        let output = try_or!(return, space.outputs().next());
        let rect = space.output_geometry(output).unwrap();

        let tool = evt.tool();
        tablet_seat.add_tool(&tool);

        self.pointer_location = evt.position_transformed(rect.size) + rect.loc.to_f64();

        let under = try_or!(return, self.surface_under());
        let tablet = try_or!(return, tablet_seat.get_tablet(&(&evt.device()).into()));
        let tool = try_or!(return, tablet_seat.get_tool(&tool));

        match evt.state() {
            ProximityState::In => tool.proximity_in(
                self.pointer_location,
                under,
                &tablet,
                scounter(),
                evt.time(),
            ),
            ProximityState::Out => tool.proximity_out(evt.time()),
        }
    }

    fn on_tablet_tool_tip<I: InputBackend>(&mut self, evt: I::TabletToolTipEvent) {
        let tool = try_or!(return, self.seat.tablet_seat().get_tool(&evt.tool()));

        match evt.tip_state() {
            TabletToolTipState::Down => {
                let serial = scounter();
                tool.tip_down(serial, evt.time());
                self.update_keyboard_focus(serial);
            }
            TabletToolTipState::Up => tool.tip_up(evt.time()),
        }
    }

    fn on_tablet_button<I: InputBackend>(&mut self, evt: I::TabletToolButtonEvent) {
        let tool = try_or!(return, self.seat.tablet_seat().get_tool(&evt.tool()));
        tool.button(evt.button(), evt.button_state(), scounter(), evt.time());
    }

    fn clamp_coords(&self, pos: Point<f64, Logical>) -> Point<f64, Logical> {
        let space = self.space.borrow();
        if space.outputs().next().is_none() {
            return pos;
        }

        let (pos_x, pos_y) = pos.into();

        // TODO: layout assumption here too

        let max_x = space
            .outputs()
            .map(|o| space.output_geometry(o).unwrap().size.w)
            .sum::<i32>();
        let clamped_x = pos_x.clamp(0.0, max_x as f64);

        let max_y = space.outputs().find_map(|o| {
            let geo = space.output_geometry(o).unwrap();
            if geo.contains((clamped_x as i32, 0)) {
                Some(geo.size.h)
            } else {
                None
            }
        });
        let clamped_y = pos_y.clamp(0.0, max_y.map_or(f64::MAX, f64::from));

        (clamped_x, clamped_y).into()
    }
}

fn process_keyboard_shortcut(modifiers: ModifiersState, keysym: Keysym) -> Option<KeyAction> {
    use xkbcommon::xkb;

    let mods = (
        modifiers.ctrl,
        modifiers.logo,
        modifiers.alt,
        modifiers.shift,
    );
    let action = match (mods, keysym) {
        ((true, _, true, _), xkb::KEY_BackSpace) | ((_, true, _, _), xkb::KEY_q) => KeyAction::Quit,

        (_, xkb::KEY_XF86Switch_VT_1..=xkb::KEY_XF86Switch_VT_12) => {
            let vt = keysym - xkb::KEY_XF86Switch_VT_1 + 1;
            KeyAction::VtSwitch(vt as i32)
        }

        ((_, true, _, _), xkb::KEY_Return) => KeyAction::Run("alacritty".into()),

        ((_, true, _, _), xkb::KEY_1..=xkb::KEY_9) => {
            let num = keysym - xkb::KEY_1;
            KeyAction::Screen(num as usize)
        }

        ((_, true, _, true), xkb::KEY_M) => KeyAction::ScaleDown,
        ((_, true, _, true), xkb::KEY_P) => KeyAction::ScaleUp,

        _ => return None,
    };
    Some(action)
}
