use std::cell::RefCell;
use std::rc::Rc;

use smithay::reexports::*;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::{
    compositor::{
        with_surface_tree_downward, SubsurfaceCachedState, SurfaceData as WlSurfaceData,
        TraversalAction,
    },
    output::{self, Mode, PhysicalProperties},
};

use slog::Logger;
use wayland_protocols::xdg_shell::server::xdg_toplevel::State as SurfaceState;
use wayland_server::{
    protocol::{wl_output, wl_surface::WlSurface},
    Display, Global, UserDataMap,
};

use crate::shell::SurfaceData;
use crate::util::with_surface_tree_downward_all;
use crate::window_map::{Kind, SurfaceTrait, WindowMap};

pub struct OutputMap {
    display: Rc<RefCell<Display>>,
    outputs: Vec<Output>,
    window_map: Rc<RefCell<WindowMap>>,
    log: Logger,
}

#[allow(dead_code)]
impl OutputMap {
    pub fn new(
        display: Rc<RefCell<Display>>,
        window_map: Rc<RefCell<WindowMap>>,
        log: Logger,
    ) -> Self {
        Self {
            display,
            outputs: Vec::new(),
            window_map,
            log,
        }
    }

    pub fn arrange(&mut self) {
        let mut x = 0;
        for output in &mut self.outputs {
            let shift = x - output.location.x;
            if shift != 0 {
                let mut window_map = self.window_map.borrow_mut();
                for surface in &output.surfaces {
                    let toplevel = try_or!(continue, window_map.find(surface));
                    let mut location = try_or!(continue, window_map.location(&toplevel));
                    if output.geometry().contains(location) {
                        location.x += shift;
                        window_map.set_location(&toplevel, location);
                    }
                }
            }

            output.location.x = x;
            output.location.y = 0;

            output
                .output
                .change_current_state(None, None, None, Some(output.location));

            x += output.size().w;
        }

        let primary_loc = self.with_primary().map_or((0, 0).into(), |o| o.location());
        let mut window_map = self.window_map.borrow_mut();

        let mut windows_to_move = Vec::new();
        let in_bounds = |bbox| self.outputs.iter().any(|o| o.geometry().overlaps(bbox));
        window_map.with_windows_from_bottom_to_top(|kind, _, &bbox| {
            if !in_bounds(bbox) {
                windows_to_move.push((kind.clone(), primary_loc));
            }
        });

        for (window, location) in windows_to_move.drain(..) {
            window_map.set_location(&window, location);
        }

        window_map.with_windows_from_bottom_to_top(|kind, location, _| {
            let xdg = match kind {
                Kind::Xdg(v) => v,
                _ => return,
            };
            let state = try_or!(return, xdg.current_state());
            if !(state.states.contains(SurfaceState::Fullscreen)
                || state.states.contains(SurfaceState::Maximized))
            {
                return;
            }

            let output = match &state.fullscreen_output {
                Some(output) => self.find_by_output(output),
                None => self.find_by_position(location),
            };

            let geometry = try_or!(return, output).geometry();
            if location != geometry.loc {
                windows_to_move.push((kind.clone(), geometry.loc));
            }

            if let Ok(_) = xdg.with_pending_state(|state| state.size = Some(geometry.size)) {
                xdg.send_configure();
            }
        });

        for (window, location) in windows_to_move {
            window_map.set_location(&window, location);
        }
    }

    pub fn add<N: AsRef<str>>(
        &mut self,
        name: N,
        props: PhysicalProperties,
        mode: Mode,
    ) -> &Output {
        let output = Output::new(
            name,
            (self.width(), 0).into(),
            &mut self.display.borrow_mut(),
            props,
            mode,
            self.log.clone(),
        );

        self.outputs.push(output);

        // the addition won't affect windows, but outputs might get reorganized.
        self.arrange();

        self.outputs.last().unwrap()
    }

    pub fn retain<F: FnMut(&Output) -> bool>(&mut self, f: F) {
        self.outputs.retain(f);
        self.arrange();
    }

    pub fn width(&self) -> i32 {
        // TODO: displays might not be lined up side-by-side like in anvil
        self.outputs.iter().map(|o| o.size().w).sum()
    }

    pub fn height(&self, x: i32) -> Option<i32> {
        // TODO: displays might not be lined up side-by-side like in anvil
        self.outputs
            .iter()
            .find(|o| {
                let g = o.geometry();
                let range = g.loc.x..(g.loc.x + g.size.w);
                range.contains(&x)
            })
            .map(|o| o.size().w)
    }

    pub fn is_empty(&self) -> bool {
        self.outputs.is_empty()
    }

    pub fn with_primary(&self) -> Option<&Output> {
        self.outputs.get(0)
    }

    pub fn find<F: FnMut(&&Output) -> bool>(&self, f: F) -> Option<&Output> {
        self.outputs.iter().find(f)
    }

    pub fn find_by_output(&self, output: &wl_output::WlOutput) -> Option<&Output> {
        self.find(|o| o.output.owns(output))
    }

    pub fn find_by_name<N: AsRef<str>>(&self, name: N) -> Option<&Output> {
        let name = name.as_ref();
        self.find(|o| o.name == name)
    }

    pub fn find_by_position(&self, position: Point<i32, Logical>) -> Option<&Output> {
        self.find(|o| o.geometry().contains(position))
    }

    pub fn find_by_index(&self, index: usize) -> Option<&Output> {
        self.outputs.get(index)
    }

    pub fn update<F: FnMut(&Output) -> bool>(
        &mut self,
        mode: Option<Mode>,
        scale: Option<f32>,
        mut f: F,
    ) {
        let output = try_or!(return, self.outputs.iter_mut().find(|o| f(&o)));

        if let Some(mode) = mode {
            output.output.delete_mode(output.current_mode);
            output
                .output
                .change_current_state(Some(mode), None, Some(output.output_scale), None);
            output.output.set_preferred(mode);
            output.current_mode = mode;
        }

        if let Some(scale) = scale {
            let factor = output.scale() / scale;

            let mut window_map = self.window_map.borrow_mut();
            for surface in &output.surfaces {
                let toplevel = try_or!(continue, window_map.find(surface));
                let location = try_or!(continue, window_map.location(&toplevel));
                let geometry = output.geometry();
                if geometry.contains(location) {
                    let mut toplevel_location = (location - geometry.loc).to_f64();
                    toplevel_location.x *= factor as f64;
                    toplevel_location.y *= factor as f64;
                    let toplevel_location = geometry.loc + toplevel_location.to_i32_round();
                    window_map.set_location(&toplevel, toplevel_location);
                }
            }
            drop(window_map);

            output.scale = scale;

            let output_scale = scale.round() as i32;
            if output.output_scale != output_scale {
                output.output_scale = output_scale;
                // TODO: Can we maybe call this one time instead of two?
                output.output.change_current_state(
                    Some(output.current_mode),
                    None,
                    Some(output_scale),
                    None,
                );
            }
        }

        self.arrange();
    }

    pub fn update_by_name<N: AsRef<str>>(
        &mut self,
        mode: Option<Mode>,
        scale: Option<f32>,
        name: N,
    ) {
        let name = name.as_ref();
        self.update(mode, scale, |o| o.name() == name)
    }

    pub fn update_scale_by_name<N: AsRef<str>>(&mut self, scale: f32, name: N) {
        self.update_by_name(None, Some(scale), name)
    }

    pub fn update_mode_by_name<N: AsRef<str>>(&mut self, mode: Mode, name: N) {
        self.update_by_name(Some(mode), None, name)
    }

    pub fn refresh(&mut self) {
        for output in &mut self.outputs {
            output.surfaces.retain(|s| s.as_ref().is_alive());
        }

        let window_map = self.window_map.clone();
        window_map
            .borrow()
            .with_windows_from_bottom_to_top(|kind, location, bbox| {
                let surface = try_or!(return, kind.get_surface());
                for output in &mut self.outputs {
                    output.refresh(surface, location, *bbox);
                }
            });
    }
}

pub struct Output {
    name: String,
    output: output::Output,
    global: Option<Global<wl_output::WlOutput>>,
    surfaces: Vec<WlSurface>,
    current_mode: Mode,
    scale: f32,
    output_scale: i32,
    location: Point<i32, Logical>,
    userdata: UserDataMap,
}

impl Output {
    fn new<N: AsRef<str>>(
        name: N,
        location: Point<i32, Logical>,
        display: &mut Display,
        props: PhysicalProperties,
        mode: Mode,
        log: Logger,
    ) -> Self {
        let name = name.as_ref().to_owned();
        let (output, global) = output::Output::new(display, name.clone(), props, log);

        let scale = display_scale(&name);
        let output_scale = scale.round() as i32;

        output.change_current_state(Some(mode), None, Some(output_scale), Some(location));
        output.set_preferred(mode);

        Self {
            name,
            global: Some(global),
            output,
            location,
            surfaces: Vec::new(),
            current_mode: mode,
            scale,
            output_scale,
            userdata: Default::default(),
        }
    }

    fn add_if_absent(&mut self, wl_surface: &WlSurface) {
        if !self.surfaces.contains(wl_surface) {
            self.output.enter(wl_surface);
            self.surfaces.push(wl_surface.clone());
        }
    }

    fn remove_if_present(&mut self, wl_surface: &WlSurface) {
        if let Some(i) = self.surfaces.iter().position(|s| s == wl_surface) {
            self.output.leave(wl_surface);
            self.surfaces.remove(i);
        }
    }

    fn refresh(
        &mut self,
        surface: &WlSurface,
        location: Point<i32, Logical>,
        bbox: Rectangle<i32, Logical>,
    ) {
        if self.geometry().overlaps(bbox) {
            let filter = |_: &_, states: &WlSurfaceData, loc: &_| {
                if states.data_map.get::<RefCell<SurfaceData>>().is_some() {
                    let mut subsurface_loc = *loc;
                    if states.role == Some("subsurface") {
                        let data = states.cached_state.current::<SubsurfaceCachedState>();
                        subsurface_loc += data.location;
                    }
                    TraversalAction::DoChildren(subsurface_loc)
                } else {
                    TraversalAction::SkipChildren
                }
            };

            let processor = |wl_surface: &_, states: &WlSurfaceData, &loc: &_| {
                let data = states.data_map.get::<RefCell<SurfaceData>>();
                let matched = if let Some(size) = data.and_then(|d| d.borrow().size()) {
                    self.geometry().overlaps(Rectangle { loc, size })
                } else {
                    false
                };

                if matched {
                    self.add_if_absent(wl_surface);
                } else {
                    self.remove_if_present(wl_surface);
                }
            };

            with_surface_tree_downward(surface, location, filter, processor, |_, _, _| true)
        } else {
            with_surface_tree_downward_all(surface, |wl_surface, _| {
                self.remove_if_present(wl_surface)
            });
        }
    }

    pub fn userdata(&self) -> &UserDataMap {
        &self.userdata
    }

    pub fn geometry(&self) -> Rectangle<i32, Logical> {
        Rectangle {
            loc: self.location(),
            size: self.size(),
        }
    }

    pub fn size(&self) -> Size<i32, Logical> {
        self.current_mode
            .size
            .to_f64()
            .to_logical(self.scale as f64)
            .to_i32_round()
    }

    pub fn location(&self) -> Point<i32, Logical> {
        self.location
    }

    pub fn scale(&self) -> f32 {
        self.scale
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn current_mode(&self) -> Mode {
        self.current_mode
    }
}

impl Drop for Output {
    fn drop(&mut self) {
        self.global.take().unwrap().destroy();
    }
}

fn display_scale(name: &str) -> f32 {
    // TODO: config file
    std::env::var(format!("WM_SCALE_{name}"))
        .as_deref()
        .unwrap_or("1.0")
        .parse::<f32>()
        .unwrap_or(1.0)
        .max(1.0)
}
