use std::{ops::Deref, sync::Mutex};

use smithay::backend::renderer::{
    gles2::{Gles2Error, Gles2Renderer, Gles2Texture},
    Frame, ImportAll, Renderer, Texture,
};
use smithay::desktop::space::{RenderElement, SpaceOutputTuple, SurfaceTree};
use smithay::reexports::*;
use smithay::utils::{Logical, Point, Rectangle, Size, Transform};
use smithay::wayland::{
    compositor::{get_role, with_states},
    seat::CursorImageAttributes,
};

use image::{ImageBuffer, Rgba};
use slog::{warn, Logger};
use wayland_server::protocol::wl_surface::WlSurface;

pub(crate) static CLEAR_COLOR: [f32; 4] = [0.8, 0.8, 0.9, 1.0];

pub(crate) fn draw_cursor_new<R, F, E, T>(
    surface: WlSurface,
    location: impl Into<Point<i32, Logical>>,
    log: &Logger,
) -> impl RenderElement<R, F, E, T>
where
    R: Renderer<Error = E, TextureId = T, Frame = F> + ImportAll,
    F: Frame<Error = E, TextureId = T>,
    T: Texture + 'static,
    E: std::error::Error,
{
    let mut position = location.into();

    if let Ok(hotspot) = with_states(&surface, |states| {
        states
            .data_map
            .get::<Mutex<CursorImageAttributes>>()
            .unwrap()
            .lock()
            .unwrap()
            .hotspot
    }) {
        position -= hotspot;
    } else {
        warn!(
            log,
            "Trying to display as a cursor a surface that does not have the CursorImage role.",
        );
    }

    SurfaceTree { surface, position }
}

pub(crate) fn draw_drag_icon_new<R, F, E, T>(
    surface: WlSurface,
    location: impl Into<Point<i32, Logical>>,
    log: &Logger,
) -> impl RenderElement<R, F, E, T>
where
    R: Renderer<Error = E, TextureId = T, Frame = F> + ImportAll,
    F: Frame<Error = E, TextureId = T>,
    T: Texture + 'static,
    E: std::error::Error,
{
    if get_role(&surface) != Some("dnd_icon") {
        warn!(
            log,
            "Trying to display an inappropriate surface as a drag icon.",
        );
    }
    let position = location.into();
    SurfaceTree { surface, position }
}

pub(crate) struct PointerElement<T> {
    texture: T,
    position: Point<i32, Logical>,
    size: Size<i32, Logical>,
}

impl<T: Texture> PointerElement<T> {
    pub(crate) fn new(texture: T, position: Point<i32, Logical>) -> Self {
        let size = texture.size().to_logical(1, Transform::Normal);
        Self {
            texture,
            position,
            size,
        }
    }
}

impl<R, F, E, T> RenderElement<R, F, E, T> for PointerElement<T>
where
    R: Renderer<Error = E, TextureId = T, Frame = F> + ImportAll,
    F: Frame<Error = E, TextureId = T>,
    T: Texture + 'static,
    E: std::error::Error,
{
    fn id(&self) -> usize {
        0
    }

    fn geometry(&self) -> Rectangle<i32, Logical> {
        Rectangle::from_loc_and_size(self.position, self.size)
    }

    fn accumulated_damage(
        &self,
        _: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Logical>> {
        vec![Rectangle::from_loc_and_size(self.position, self.size)]
    }

    fn draw(
        &self,
        _renderer: &mut R,
        frame: &mut F,
        scale: f64,
        location: Point<i32, Logical>,
        damage: &[Rectangle<i32, Logical>],
        _log: &Logger,
    ) -> Result<(), E> {
        let pos = location.to_f64().to_physical(scale).to_i32_round();

        let damage = damage
            .iter()
            .map(|rect| rect.to_buffer(1, Transform::Normal, &self.size))
            .collect::<Vec<_>>();

        frame.render_texture_at(
            &self.texture,
            pos,
            1,
            scale,
            Transform::Normal,
            &damage,
            1.0,
        )
    }
}

#[cfg(feature = "debug")]
pub(crate) static FPS_NUMBERS_PNG: &[u8] = include_bytes!("../resources/numbers.png");

#[cfg(feature = "debug")]
pub(crate) struct FpsElement<T> {
    value: u32,
    texture: T,
}

#[cfg(feature = "debug")]
impl<R, F, E, T> RenderElement<R, F, E, T> for FpsElement<T>
where
    R: Renderer<Error = E, TextureId = T, Frame = F> + ImportAll,
    F: Frame<Error = E, TextureId = T>,
    T: Texture + 'static,
    E: std::error::Error,
{
    fn id(&self) -> usize {
        0
    }

    fn geometry(&self) -> Rectangle<i32, Logical> {
        let digits = match self.value {
            0..=9 => 1,
            10..=99 => 2,
            _ => 3,
        };
        Rectangle::from_loc_and_size((0, 0), (24 * digits, 35))
    }

    fn accumulated_damage(
        &self,
        _: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Logical>> {
        vec![Rectangle::from_loc_and_size((0, 0), (24 * 3, 35))]
    }

    fn draw(
        &self,
        _renderer: &mut R,
        frame: &mut F,
        scale: f64,
        location: Point<i32, Logical>,
        damage: &[Rectangle<i32, Logical>],
        _log: &Logger,
    ) -> Result<(), E> {
        let location = location.to_f64().to_physical(scale);

        let area: Size<i32, Logical> = (22, 35).into();

        for (i, digit) in get_digits(self.value).into_iter().enumerate() {
            let offset_x = location.x + i as f64 * 24.0 * scale;

            let r = Rectangle::from_loc_and_size(((offset_x / scale) as i32, 0), area);
            let damage = damage
                .iter()
                .flat_map(|x| x.intersection(r))
                .map(|mut x| {
                    x.loc = (0, 0).into();
                    x.to_buffer(1, Transform::Normal, &area)
                })
                .collect::<Vec<_>>();

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
            let src_area = area.to_buffer(1, Transform::Normal);

            let dst_loc = (offset_x, location.y);
            let dst_area = area.to_f64().to_physical(scale);

            frame.render_texture_from_to(
                &self.texture,
                Rectangle::from_loc_and_size(src_loc, src_area),
                Rectangle::from_loc_and_size(dst_loc, dst_area),
                &damage,
                Transform::Normal,
                1.0,
            )?;
        }
        Ok(())
    }
}

#[cfg(feature = "debug")]
pub(crate) fn draw_fps_new<R, F, E, T>(texture: &T, value: u32) -> impl RenderElement<R, F, E, T>
where
    R: Renderer<Error = E, TextureId = T, Frame = F> + ImportAll,
    F: Frame<Error = E, TextureId = T>,
    T: Texture + Clone + 'static,
    E: std::error::Error,
{
    let texture = texture.clone();
    FpsElement { value, texture }
}

// TODO: a more general "draw context bundle" kinda struct.
// these functions' signatures are very similar.
pub(crate) trait RendererAndFrame<'r, 'f> {
    type R: 'r;
    type F: 'f;
    fn pair(self) -> (&'r mut Self::R, &'f mut Self::F);
}

impl<'r, 'f, R, F> RendererAndFrame<'r, 'f> for (&'r mut R, &'f mut F)
where
    R: 'r,
    F: 'f,
{
    type R = R;
    type F = F;
    fn pair(self) -> (&'r mut R, &'f mut F) {
        self
    }
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

pub(crate) fn import_bitmap<C: Deref<Target = [u8]>>(
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
