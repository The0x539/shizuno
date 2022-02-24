use std::{cell::RefCell, ops::Deref, sync::Mutex};

use smithay::backend::{
    renderer::{
        buffer_type,
        gles2::{Gles2Error, Gles2Renderer, Gles2Texture},
        BufferType, Frame, ImportAll, Renderer, Texture,
    },
    SwapBuffersError,
};
use smithay::reexports::*;
use smithay::utils::{Logical, Point, Rectangle, Transform};
use smithay::wayland::{
    compositor::{
        get_role, with_states, with_surface_tree_upward, Damage, SubsurfaceCachedState,
        SurfaceAttributes, TraversalAction,
    },
    seat::CursorImageAttributes,
    shell::wlr_layer::Layer,
};

use image::{ImageBuffer, Rgba};
use slog::{error, warn, Logger};
use wayland_server::protocol::{wl_buffer::WlBuffer, wl_surface::WlSurface};

use crate::{
    shell::SurfaceData,
    window_map::{LayerSurface, SurfaceTrait, WindowMap},
};

// TODO: a more general "draw context bundle" kinda struct.
// these functions' signatures are very similar.
pub trait RendererAndFrame<'r, 'f> {
    type R: Renderer<Error = Self::E, TextureId = Self::T, Frame = Self::F> + ImportAll + 'r;
    type F: Frame<Error = Self::E, TextureId = Self::T> + 'f;
    type E: std::error::Error + Into<SwapBuffersError>;
    type T: Texture + 'static;
    fn pair(self) -> (&'r mut Self::R, &'f mut Self::F);
}

impl<'r, 'f, R, E, F, T> RendererAndFrame<'r, 'f> for (&'r mut R, &'f mut F)
where
    R: Renderer<Error = E, TextureId = T, Frame = F> + ImportAll + 'r,
    F: Frame<Error = E, TextureId = T> + 'f,
    E: std::error::Error + Into<SwapBuffersError>,
    T: Texture + 'static,
{
    type R = R;
    type F = F;
    type E = E;
    type T = T;
    fn pair(self) -> (&'r mut R, &'f mut F) {
        self
    }
}

struct BufferTextures<T> {
    buffer: Option<WlBuffer>,
    texture: T,
}

impl<T> Drop for BufferTextures<T> {
    fn drop(&mut self) {
        if let Some(buffer) = self.buffer.take() {
            buffer.release();
        }
    }
}

pub fn draw_cursor<'r, 'e>(
    r_f: impl RendererAndFrame<'r, 'e>,
    surface: &WlSurface,
    location: Point<i32, Logical>,
    output_scale: f32,
    log: &Logger,
) -> Result<(), SwapBuffersError> {
    let delta = with_states(surface, |states| {
        type Mcia = Mutex<CursorImageAttributes>;
        let data = states.data_map.get::<Mcia>().unwrap().lock().unwrap();
        data.hotspot
    })
    .unwrap_or_else(|_| {
        warn!(
            log,
            "Trying to display as a cursor a surface that does not have the CursorImage role.",
        );
        (0, 0).into()
    });
    draw_surface_tree(r_f, surface, location - delta, output_scale, log)
}

pub fn full<T>() -> [Rectangle<i32, T>; 1] {
    [Rectangle::from_loc_and_size((0, 0), (i32::MAX, i32::MAX))]
}

macro_rules! try_or_skip {
    ($e:expr) => {
        try_or!(return TraversalAction::SkipChildren, $e)
    };
}

fn draw_surface_tree<'r, 'e, Tex: 'static>(
    r_f: impl RendererAndFrame<'r, 'e, T = Tex>,
    root: &WlSurface,
    location: Point<i32, Logical>,
    output_scale: f32,
    log: &Logger,
) -> Result<(), SwapBuffersError> {
    let (renderer, frame) = r_f.pair();
    let mut result = Ok(());

    with_surface_tree_upward(
        root,
        location,
        |_surface, states, location| {
            let mut location = *location;

            let data = try_or_skip!(states.data_map.get::<RefCell<SurfaceData>>());

            let mut data = data.borrow_mut();
            let attributes = states.cached_state.current::<SurfaceAttributes>();
            if data.texture.is_none() {
                let buffer = try_or_skip!(data.buffer.take());

                let damage = attributes
                    .damage
                    .iter()
                    .map(|dmg| match dmg {
                        Damage::Buffer(rect) => *rect,
                        // TODO: also apply transformations
                        Damage::Surface(rect) => rect.to_buffer(
                            attributes.buffer_scale,
                            attributes.buffer_transform.into(),
                            &data.size().unwrap(),
                        ),
                    })
                    .collect::<Vec<_>>();

                match renderer.import_buffer(&buffer, Some(states), &damage) {
                    Some(Ok(texture)) => {
                        let buffer = match buffer_type(&buffer) {
                            Some(BufferType::Shm) => {
                                buffer.release();
                                None
                            }
                            _ => Some(buffer),
                        };
                        data.texture = Some(Box::new(BufferTextures { buffer, texture }))
                    }
                    Some(Err(err)) => {
                        warn!(log, "Error loading buffer: {err:?}");
                        buffer.release();
                    }
                    None => {
                        error!(log, "Unknown buffer format for: {buffer:?}");
                        buffer.release();
                    }
                }
            }

            if data.texture.is_some() {
                if states.role == Some("subsurface") {
                    let current = states.cached_state.current::<SubsurfaceCachedState>();
                    location += current.location;
                }
                TraversalAction::DoChildren(location)
            } else {
                TraversalAction::SkipChildren
            }
        },
        |_surface, states, location| {
            let mut location = *location;
            let data = try_or!(return, states.data_map.get::<RefCell<SurfaceData>>());
            let mut data = data.borrow_mut();
            let buffer_scale = data.buffer_scale;
            let buffer_transform = data.buffer_transform;

            let texture = try_or!(return, data.texture.as_mut());
            let texture = try_or!(return, texture.downcast_mut::<BufferTextures<Tex>>());

            if states.role == Some("subsurface") {
                let current = states.cached_state.current::<SubsurfaceCachedState>();
                location += current.location;
            }

            if let Err(e) = frame.render_texture_at(
                &texture.texture,
                location
                    .to_f64()
                    .to_physical(output_scale as f64)
                    .to_i32_round(),
                buffer_scale,
                output_scale as f64,
                buffer_transform,
                &full(),
                1.0,
            ) {
                result = Err(e.into());
            }
        },
        |_, _, _| true,
    );

    result
}

pub fn draw_layers<'r, 'e>(
    r_f: impl RendererAndFrame<'r, 'e>,
    window_map: &WindowMap,
    layer: Layer,
    output_rect: Rectangle<i32, Logical>,
    output_scale: f32,
    log: &Logger,
) -> Result<(), SwapBuffersError> {
    let (renderer, frame) = r_f.pair();

    let mut result = Ok(());
    let f = |layer_surface: &LayerSurface| {
        // skip layers that do not overlap with a given output
        if !output_rect.overlaps(layer_surface.bbox) {
            return;
        }

        let mut initial_place: Point<i32, Logical> = layer_surface.location;
        initial_place.x -= output_rect.loc.x;

        let wl_surface = try_or!(return, layer_surface.surface.get_surface());

        if let Err(e) = draw_surface_tree(
            (&mut *renderer, &mut *frame),
            wl_surface,
            initial_place,
            output_scale,
            log,
        ) {
            result = Err(e);
        }

        window_map.with_child_popups(wl_surface, |popup| {
            let location = popup.location();
            let draw_location = initial_place + location;
            let wl_surface = try_or!(return, popup.get_surface());
            if let Err(e) = draw_surface_tree(
                (&mut *renderer, &mut *frame),
                wl_surface,
                draw_location,
                output_scale,
                log,
            ) {
                result = Err(e);
            }
        });
    };

    window_map.layers.with_layers_from_bottom_to_top(&layer, f);

    result
}

pub fn draw_drag_icon<'r, 'e>(
    r_f: impl RendererAndFrame<'r, 'e>,
    surface: &WlSurface,
    location: Point<i32, Logical>,
    output_scale: f32,
    log: &Logger,
) -> Result<(), SwapBuffersError> {
    if get_role(surface) != Some("dnd_icon") {
        warn!(
            log,
            "Trying to display an inappropriate surface as a drag icon.",
        );
    }
    draw_surface_tree(r_f, surface, location, output_scale, log)
}

#[cfg(feature = "debug")]
pub static FPS_NUMBERS_PNG: &[u8] = include_bytes!("../resources/numbers.png");

pub fn draw_windows<'r, 'e>(
    r_f: impl RendererAndFrame<'r, 'e>,
    window_map: &WindowMap,
    output_rect: Rectangle<i32, Logical>,
    output_scale: f32,
    log: &Logger,
) -> Result<(), SwapBuffersError> {
    let (renderer, frame) = r_f.pair();
    let mut result = Ok(());

    window_map.with_windows_from_bottom_to_top(|root_surface, mut initial_place, &bounding_box| {
        if !output_rect.overlaps(bounding_box) {
            return;
        }

        initial_place.x -= output_rect.loc.x;
        let wl_surface = try_or!(return, root_surface.get_surface());

        if let Err(e) = draw_surface_tree(
            (&mut *renderer, &mut *frame),
            wl_surface,
            initial_place,
            output_scale,
            log,
        ) {
            result = Err(e);
        }

        let toplevel_geometry_offset = window_map.geometry(root_surface).unwrap_or_default().loc;

        window_map.with_child_popups(wl_surface, |popup| {
            let draw_location = initial_place + popup.location() + toplevel_geometry_offset;
            let wl_surface = try_or!(return, popup.get_surface());

            if let Err(e) = draw_surface_tree(
                (&mut *renderer, &mut *frame),
                wl_surface,
                draw_location,
                output_scale,
                log,
            ) {
                result = Err(e);
            }
        });
    });

    result
}

fn get_digits(mut x: u32) -> Vec<u8> {
    if x <= 9 {
        return vec![x as u8];
    }

    let mut v = Vec::with_capacity(10);

    loop {
        let digit = x % 10;
        x /= 10;
        v.push(digit as u8);
        if x == 0 {
            v.reverse();
            return v;
        }
    }
}

#[cfg(feature = "debug")]
pub fn draw_fps<'r, 'e, Tex: 'static>(
    r_f: impl RendererAndFrame<'r, 'e, T = Tex>,
    texture: &Tex,
    scale: f64,
    value: u32,
) -> Result<(), SwapBuffersError> {
    let (_renderer, frame) = r_f.pair();

    for (i, digit) in get_digits(value).into_iter().enumerate() {
        let src_loc = match digit {
            9 => (0, 0),
            6 => (22, 0),
            3 => (44, 0),
            1 => (66, 0),

            8 => (0, 35),
            0 => (22, 35),
            2 => (44, 35),

            7 => (0, 70),
            4 => (22, 70),
            5 => (44, 70),

            _ => unreachable!(),
        };
        let src = Rectangle::from_loc_and_size(src_loc, (22, 35));
        let dst = Rectangle::from_loc_and_size(
            (i as f64 * 24.0 * scale, 0.0),
            (22.0 * scale, 35.0 * scale),
        );
        frame
            .render_texture_from_to(texture, src, dst, &full(), Transform::Normal, 1.0)
            .map_err(Into::into)?;
    }

    Ok(())
}

pub fn import_bitmap<C: Deref<Target = [u8]>>(
    renderer: &mut Gles2Renderer,
    image: &ImageBuffer<Rgba<u8>, C>,
) -> Result<Gles2Texture, Gles2Error> {
    use smithay::backend::renderer::gles2::ffi;

    let (w, h) = (image.width() as i32, image.height() as i32);
    let clamp = ffi::CLAMP_TO_EDGE as i32;

    renderer.with_context(|renderer, gl| unsafe {
        let mut tex = 0;
        gl.GenTextures(1, &mut tex);
        gl.BindTexture(ffi::TEXTURE_2D, tex);
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, clamp);
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, clamp);
        gl.TexImage2D(
            ffi::TEXTURE_2D,
            0,
            ffi::RGBA as i32,
            w,
            h,
            0,
            ffi::RGBA,
            ffi::UNSIGNED_BYTE as u32,
            image.as_ptr() as *const _,
        );
        gl.BindTexture(ffi::TEXTURE_2D, 0);

        Gles2Texture::from_raw(renderer, tex, (w, h).into())
    })
}
