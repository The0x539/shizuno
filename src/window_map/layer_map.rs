use std::cell::RefCell;
use std::time::Duration;

use smithay::reexports::*;
use smithay::utils::{Logical, Point, Rectangle};
use smithay::wayland::{
    compositor::{with_states, with_surface_tree_downward, SubsurfaceCachedState, TraversalAction},
    shell::wlr_layer::{Anchor, Layer, LayerSurface as WlrLayerSurface, LayerSurfaceCachedState},
};

use once_cell::unsync::OnceCell;
use wayland_server::protocol::wl_surface::WlSurface;

use crate::output_map::Output;
use crate::shell::SurfaceData;
use crate::util::with_surface_tree_downward_all;

pub struct LayerSurface {
    pub surface: WlrLayerSurface,
    pub location: Point<i32, Logical>,
    pub bbox: Rectangle<i32, Logical>,
    layer: Layer,
}

impl LayerSurface {
    fn matching(&self, point: Point<f64, Logical>) -> Option<(WlSurface, Point<i32, Logical>)> {
        if !self.bbox.to_f64().contains(point) {
            return None;
        }

        let wl_surface = self.surface.get_surface()?;

        let found = OnceCell::new();
        // TODO: the repetition of this logic between here and window_map is bad
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
        let wl_surface = try_or!(return, self.surface.get_surface());
        with_surface_tree_downward(
            wl_surface,
            self.location,
            |_, states, loc| {
                let mut loc = *loc;

                let data = try_or!(
                    return TraversalAction::SkipChildren,
                    states.data_map.get::<RefCell<SurfaceData>>(),
                );
                let size = try_or!(return TraversalAction::SkipChildren, data.borrow().size());

                if states.role == Some("subsurface") {
                    let current = states.cached_state.current::<SubsurfaceCachedState>();
                    loc += current.location;
                }

                self.bbox = self.bbox.merge(Rectangle { loc, size });
                TraversalAction::DoChildren(loc)
            },
            |_, _, _| (),
            |_, _, _| true,
        );

        self.layer = with_states(wl_surface, |states| {
            let current = states.cached_state.current::<LayerSurfaceCachedState>();
            current.layer
        })
        .unwrap();
    }

    fn send_frame(&self, time: Duration) {
        let wl_surface = try_or!(return, self.surface.get_surface());
        with_surface_tree_downward_all(wl_surface, |_, states| {
            SurfaceData::send_frame(&mut states.cached_state.current(), time);
        });
    }
}

#[derive(Default)]
pub struct LayerMap {
    surfaces: Vec<LayerSurface>,
}

impl LayerMap {
    pub fn insert(&mut self, surface: WlrLayerSurface, layer: Layer) {
        let (location, bbox) = Default::default();
        let mut layer = LayerSurface {
            surface,
            location,
            bbox,
            layer,
        };
        layer.self_update();
        self.surfaces.insert(0, layer);
    }

    pub fn get_surface_under(
        &self,
        layer: &Layer,
        point: Point<f64, Logical>,
    ) -> Option<(WlSurface, Point<i32, Logical>)> {
        self.surfaces
            .iter()
            .filter(|s| s.layer == *layer)
            .find_map(|s| s.matching(point))
    }

    pub fn with_layers_from_bottom_to_top<F: FnMut(&LayerSurface)>(&self, layer: &Layer, f: F) {
        self.surfaces
            .iter()
            .rev()
            .filter(|s| s.layer == *layer)
            .for_each(f)
    }

    pub fn refresh(&mut self) {
        self.surfaces.retain(|l| l.surface.alive());
        for l in &mut self.surfaces {
            l.self_update();
        }
    }

    pub fn find(&self, surface: &WlSurface) -> Option<&LayerSurface> {
        self.surfaces
            .iter()
            .find(|l| l.surface.get_surface() == Some(surface))
    }

    pub fn arrange_layers(&mut self, output: &Output) {
        let output_rect = output.geometry();

        for layer in &mut self.surfaces {
            let surface = try_or!(continue, layer.surface.get_surface());
            if !output.layer_surfaces().contains(surface) {
                continue;
            }

            let data = with_states(surface, |states| {
                *states.cached_state.current::<LayerSurfaceCachedState>()
            })
            .unwrap();

            let x = if data.size.w == 0 || data.anchor.contains(Anchor::LEFT) {
                output_rect.loc.x
            } else if data.anchor.contains(Anchor::RIGHT) {
                output_rect.loc.x + (output_rect.size.w - data.size.w)
            } else {
                output_rect.loc.x + ((output_rect.size.w / 2) - (data.size.w / 2))
            };

            let y = if data.size.h == 0 || data.anchor.contains(Anchor::TOP) {
                output_rect.loc.y
            } else if data.anchor.contains(Anchor::BOTTOM) {
                output_rect.loc.y + (output_rect.size.h - data.size.h)
            } else {
                output_rect.loc.y + ((output_rect.size.h / 2) - (data.size.h / 2))
            };

            layer.location = (x, y).into();

            layer
                .surface
                .with_pending_state(|state| state.size = Some(output_rect.size))
                .unwrap();
            layer.surface.send_configure();
        }
    }

    pub fn send_frames(&self, time: Duration) {
        for layer in &self.surfaces {
            layer.send_frame(time);
        }
    }
}
