use std::process::Command;

use smithay::backend::{
    input::{
        self, Device, DeviceCapability, Event, InputBackend, InputEvent, KeyState,
        KeyboardKeyEvent, PointerAxisEvent, PointerButtonEvent, PointerMotionEvent, ProximityState,
        TabletToolButtonEvent, TabletToolEvent, TabletToolProximityEvent, TabletToolTipEvent,
        TabletToolTipState,
    },
    session::Session,
};
use smithay::reexports::*;
use smithay::utils::{Logical, Point};
use smithay::wayland::{
    seat::{AxisFrame, FilterResult, ModifiersState},
    tablet_manager::TabletSeatTrait,
    Serial,
};

use slog::{debug, error, info};
use wayland_server::protocol::wl_pointer;
use xkbcommon::xkb::Keysym;

use crate::{state::State, udev::UdevData};

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

impl<B> State<B> {
    fn keyboard_key_to_action<I: InputBackend>(&mut self, evt: I::KeyboardKeyEvent) -> KeyAction {
        let keycode = evt.key_code();
        let state = evt.state();
        debug!(self.log, "key"; "keycode" => keycode, "state" => format!("{state:?}"));

        self.keyboard
            .input(
                keycode,
                state,
                scounter(),
                evt.time(),
                // TODO: de-inline this closure again once it's possible to name `handle`'s datatype
                |modifiers, handle| {
                    let keysym = handle.modified_sym();

                    debug!(self.log, "keysym";
                        "state" => format!("{state:?}"),
                        "mods" => format!("{modifiers:?}"),
                        "keysym" => xkbcommon::xkb::keysym_get_name(keysym)
                    );

                    if state == KeyState::Pressed {
                        if let Some(action) = process_keyboard_shortcut(*modifiers, keysym) {
                            self.suppressed_keys.insert(keysym);
                            FilterResult::Intercept(action)
                        } else {
                            FilterResult::Forward
                        }
                    } else {
                        if self.suppressed_keys.remove(&keysym) {
                            FilterResult::Intercept(KeyAction::None)
                        } else {
                            FilterResult::Forward
                        }
                    }
                },
            )
            .unwrap_or(KeyAction::None)
    }

    fn change_scale(&mut self, delta: f32) {
        let mut output_map = self.output_map.borrow_mut();

        let ptr_loc = self.pointer_location.to_i32_round();
        let output = try_or!(return, output_map.find_by_position(ptr_loc));

        let scale = output.scale();
        let output_location = output.location().to_f64();
        let name = output.name().to_owned();

        let new_scale = (scale + delta).max(1.0);

        output_map.update_scale_by_name(new_scale, name);

        let factor = scale as f64 / new_scale as f64;
        let mut pointer_output_location = self.pointer_location - output_location;
        pointer_output_location.x *= factor;
        pointer_output_location.y *= factor;
        self.pointer_location = output_location + pointer_output_location;
        let ptr_loc = self.pointer_location;

        let under = self.window_map.borrow().get_surface_under(ptr_loc);
        self.pointer.motion(ptr_loc, under, scounter(), 0);
    }

    fn on_pointer_button<I: InputBackend>(&mut self, evt: I::PointerButtonEvent) {
        let serial = scounter();

        if evt.state() == input::ButtonState::Pressed && !self.pointer.is_grabbed() {
            let under = self
                .window_map
                .borrow_mut()
                .get_surface_and_bring_to_top(self.pointer_location);
            let surface = under.as_ref().map(|s| &s.0);
            self.keyboard.set_focus(surface, serial);
        }

        let state = match evt.state() {
            input::ButtonState::Pressed => wl_pointer::ButtonState::Pressed,
            input::ButtonState::Released => wl_pointer::ButtonState::Released,
        };
        let button = match evt.button() {
            input::MouseButton::Left => 0x110,
            input::MouseButton::Right => 0x111,
            input::MouseButton::Middle => 0x112,
            input::MouseButton::Other(b) => b as u32,
        };
        self.pointer.button(button, state, serial, evt.time());
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
                    let output_map = self.output_map.borrow();
                    let output = try_or!(return, output_map.find_by_index(num));
                    let geometry = output.geometry();
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

        let under = self.window_map.borrow().get_surface_under(ptr_loc);
        self.pointer.motion(ptr_loc, under, serial, evt.time());
    }

    fn on_tablet_tool_axis<I: InputBackend>(&mut self, evt: I::TabletToolAxisEvent) {
        let output_map = self.output_map.borrow();
        let tablet_seat = self.seat.tablet_seat();
        let window_map = self.window_map.borrow();

        let output = try_or!(return, output_map.with_primary());
        let rect = output.geometry();

        self.pointer_location = evt.position_transformed(rect.size) + rect.loc.to_f64();

        let under = window_map.get_surface_under(self.pointer_location);

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
        let output_map = self.output_map.borrow();
        let tablet_seat = self.seat.tablet_seat();
        let window_map = self.window_map.borrow();

        let output = try_or!(return, output_map.with_primary());
        let rect = output.geometry();

        let tool = evt.tool();
        tablet_seat.add_tool(&tool);

        self.pointer_location = evt.position_transformed(rect.size) + rect.loc.to_f64();

        let under = try_or!(return, window_map.get_surface_under(self.pointer_location));
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
                tool.tip_down(scounter(), evt.time());

                if !self.pointer.is_grabbed() {
                    let under = self
                        .window_map
                        .borrow_mut()
                        .get_surface_and_bring_to_top(self.pointer_location);

                    let surface = under.as_ref().map(|s| &s.0);
                    self.keyboard.set_focus(surface, scounter());
                }
            }
            TabletToolTipState::Up => tool.tip_up(evt.time()),
        }
    }

    fn on_tablet_button<I: InputBackend>(&mut self, evt: I::TabletToolButtonEvent) {
        let tool = try_or!(return, self.seat.tablet_seat().get_tool(&evt.tool()));
        tool.button(evt.button(), evt.button_state(), scounter(), evt.time());
    }

    fn clamp_coords(&self, pos: Point<f64, Logical>) -> Point<f64, Logical> {
        if self.output_map.borrow().is_empty() {
            return pos;
        }

        let (pos_x, pos_y) = pos.into();
        let output_map = self.output_map.borrow();

        let max_x = output_map.width();
        let clamped_x = pos_x.clamp(0.0, max_x as f64);

        let max_y = output_map.height(clamped_x as i32);
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
