use std::cell::RefCell;
use std::sync::Mutex;
use std::time::Duration;

use smithay::reexports::*;
use smithay::utils::{Logical, Point, Rectangle};
use smithay::wayland::{
    compositor::{with_states, with_surface_tree_downward, SubsurfaceCachedState, TraversalAction},
    shell::{
        legacy::ShellSurface,
        xdg::{PopupSurface, SurfaceCachedState, ToplevelSurface, XdgPopupSurfaceRoleAttributes},
    },
};

use enum_dispatch::enum_dispatch;
use once_cell::unsync::OnceCell;
use wayland_protocols::xdg_shell::server::xdg_toplevel;
use wayland_server::protocol::wl_surface::WlSurface;

use crate::shell::SurfaceData;
use crate::util::with_surface_tree_downward_all;
use crate::xwayland::X11Surface;

#[enum_dispatch]
pub trait SurfaceTrait {
    fn alive(&self) -> bool;
    fn get_surface(&self) -> Option<&WlSurface>;
}

#[rustfmt::skip]
macro_rules! impl_surface_trait {
    ($t:ty) => {
        impl SurfaceTrait for $t {
            fn alive(&self) -> bool { self.alive() }
            fn get_surface(&self) -> Option<&WlSurface> { self.get_surface() }
        }
    };
}

impl_surface_trait!(ToplevelSurface);
impl_surface_trait!(ShellSurface);
impl_surface_trait!(PopupSurface);

#[derive(Clone, PartialEq)]
#[enum_dispatch(SurfaceTrait)]
pub enum Kind {
    Xdg(ToplevelSurface),
    Wl(ShellSurface),
    X11(X11Surface),
}

impl Kind {
    pub fn set_activated(&self, active: bool) {
        if let Kind::Xdg(t) = self {
            let changed = t.with_pending_state(|state| {
                if active {
                    state.states.set(xdg_toplevel::State::Activated)
                } else {
                    state.states.unset(xdg_toplevel::State::Activated)
                }
            });
            if let Ok(true) = changed {
                t.send_configure();
            }
        }
    }
}

#[derive(Clone)]
#[enum_dispatch(SurfaceTrait)]
pub enum PopupKind {
    Xdg(PopupSurface),
}

// The absurdity of this alias reflects the absurdity of its referent.
type Mxpsra = Mutex<XdgPopupSurfaceRoleAttributes>;

impl PopupKind {
    fn parent(&self) -> Option<WlSurface> {
        let wl_surface = self.get_surface()?;
        with_states(wl_surface, |states| {
            let attrs = states.data_map.get::<Mxpsra>().unwrap().lock().unwrap();
            attrs.parent.clone()
        })
        .ok()
        .flatten()
    }

    pub fn location(&self) -> Point<i32, Logical> {
        let wl_surface = try_or!(return (0, 0).into(), self.get_surface());
        with_states(wl_surface, |states| {
            let attrs = states.data_map.get::<Mxpsra>().unwrap().lock().unwrap();
            attrs.current.geometry
        })
        .unwrap_or_default()
        .loc
    }
}

#[derive(Default)]
pub struct WindowMap {
    windows: Vec<Window>,
    popups: Vec<Popup>,
}

impl WindowMap {
    pub fn insert(&mut self, toplevel: Kind, location: Point<i32, Logical>) {
        let mut window = Window {
            location,
            bbox: Rectangle::default(),
            toplevel,
        };
        window.self_update();
        // TODO: deque?
        self.windows.insert(0, window);
    }

    #[allow(dead_code)] // only used in... wlcs_anvil?
    pub fn windows(&self) -> impl Iterator<Item = Kind> + '_ {
        self.windows.iter().map(|w| w.toplevel.clone())
    }

    pub fn insert_popup(&mut self, popup: PopupKind) {
        self.popups.push(Popup { popup });
    }

    pub fn get_surface_under(
        &self,
        point: Point<f64, Logical>,
    ) -> Option<(WlSurface, Point<i32, Logical>)> {
        self.windows.iter().find_map(|w| w.matching(point))
    }

    pub fn get_surface_and_bring_to_top(
        &mut self,
        point: Point<f64, Logical>,
    ) -> Option<(WlSurface, Point<i32, Logical>)> {
        let mut found = None;
        for (i, window) in self.windows.iter().enumerate() {
            if let Some(surface) = window.matching(point) {
                found = Some((i, surface));
                break;
            }
        }
        let (i, surface) = found?;

        // activate the winner and only the winner
        for (j, window) in self.windows.iter().enumerate() {
            window.toplevel.set_activated(i == j);
        }

        // move the winner to the front of the list
        let winner = self.windows.remove(i);
        self.windows.insert(0, winner);

        Some(surface)
    }

    pub fn with_windows_from_bottom_to_top<F>(&self, mut f: F)
    where
        F: FnMut(&Kind, Point<i32, Logical>, &Rectangle<i32, Logical>),
    {
        for w in self.windows.iter().rev() {
            f(&w.toplevel, w.location, &w.bbox);
        }
    }

    pub fn with_child_popups<F: FnMut(&PopupKind)>(&self, base: &WlSurface, mut f: F) {
        for w in self.popups.iter().rev() {
            if w.popup.parent().as_ref() == Some(base) {
                f(&w.popup);
            }
        }
    }

    pub fn refresh(&mut self) {
        self.windows.retain(|w| w.toplevel.alive());
        self.popups.retain(|p| p.popup.alive());
        for w in &mut self.windows {
            w.self_update();
        }
    }

    pub fn refresh_toplevel(&mut self, toplevel: &Kind) {
        if let Some(w) = self.windows.iter_mut().find(|w| &w.toplevel == toplevel) {
            w.self_update();
        }
    }

    pub fn clear(&mut self) {
        self.windows.clear();
    }

    pub fn find(&self, surface: &WlSurface) -> Option<Kind> {
        for window in &self.windows {
            let s = try_or!(continue, window.toplevel.get_surface());
            if s.as_ref().equals(surface.as_ref()) {
                return Some(window.toplevel.clone());
            }
        }
        None
    }

    pub fn find_popup(&self, surface: &WlSurface) -> Option<PopupKind> {
        for p in &self.popups {
            let s = try_or!(continue, p.popup.get_surface());
            if s.as_ref().equals(surface.as_ref()) {
                return Some(p.popup.clone());
            }
        }
        None
    }

    pub fn location(&self, toplevel: &Kind) -> Option<Point<i32, Logical>> {
        self.windows
            .iter()
            .find(|w| w.toplevel == *toplevel)
            .map(|w| w.location)
    }

    pub fn set_location(&mut self, toplevel: &Kind, location: Point<i32, Logical>) {
        if let Some(window) = self.windows.iter_mut().find(|w| w.toplevel == *toplevel) {
            window.location = location;
            window.self_update();
        }
    }

    pub fn geometry(&self, toplevel: &Kind) -> Option<Rectangle<i32, Logical>> {
        self.windows
            .iter()
            .find(|w| &w.toplevel == toplevel)
            .map(|w| w.geometry())
    }

    pub fn send_frames(&self, time: Duration) {
        for window in &self.windows {
            window.send_frame(time);
        }
    }
}

struct Window {
    location: Point<i32, Logical>,
    bbox: Rectangle<i32, Logical>,
    toplevel: Kind,
}

impl Window {
    fn matching(&self, point: Point<f64, Logical>) -> Option<(WlSurface, Point<i32, Logical>)> {
        if !self.bbox.to_f64().contains(point) {
            return None;
        }

        let wl_surface = self.toplevel.get_surface()?;

        let found = OnceCell::new();
        with_surface_tree_downward(
            wl_surface,
            self.location,
            |wl_surface, states, location| {
                let mut location = *location;
                if states.role == Some("subsurface") {
                    let current = states.cached_state.current::<SubsurfaceCachedState>();
                    location += current.location;
                }

                if let Some(data) = states.data_map.get::<RefCell<SurfaceData>>() {
                    let attrs = states.cached_state.current();
                    let point = point - location.to_f64();
                    if data.borrow().contains_point(&attrs, point) {
                        let _ = found.set((wl_surface.clone(), location));
                    }
                }

                TraversalAction::DoChildren(location)
            },
            |_, _, _| (),
            |_, _, _| found.get().is_some(),
        );
        found.into_inner()
    }

    fn self_update(&mut self) {
        self.bbox = Rectangle::from_loc_and_size(self.location, (0, 0));
        let wl_surface = try_or!(return, self.toplevel.get_surface());

        with_surface_tree_downward(
            wl_surface,
            self.location,
            |_, states, loc| {
                let data = states.data_map.get::<RefCell<SurfaceData>>();
                if let Some(size) = data.and_then(|d| d.borrow().size()) {
                    let mut loc = *loc;
                    if states.role == Some("subsurface") {
                        let current = states.cached_state.current::<SubsurfaceCachedState>();
                        loc += current.location;
                    }

                    self.bbox = self.bbox.merge(Rectangle { loc, size });

                    TraversalAction::DoChildren(loc)
                } else {
                    // if the parent surface is unmapped, then so are the children
                    TraversalAction::SkipChildren
                }
            },
            |_, _, _| (),
            |_, _, _| true,
        );
    }

    fn geometry(&self) -> Rectangle<i32, Logical> {
        with_states(self.toplevel.get_surface().unwrap(), |states| {
            states.cached_state.current::<SurfaceCachedState>().geometry
        })
        .unwrap()
        .unwrap_or(self.bbox)
    }

    fn send_frame(&self, time: Duration) {
        if let Some(wl_surface) = self.toplevel.get_surface() {
            with_surface_tree_downward_all(wl_surface, |_, states| {
                SurfaceData::send_frame(&mut states.cached_state.current(), time)
            });
        }
    }
}

pub struct Popup {
    popup: PopupKind,
}
