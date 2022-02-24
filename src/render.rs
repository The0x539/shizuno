use smithay::backend::renderer::{Frame, ImportAll, Renderer};
use smithay::desktop::{
    draw_window,
    space::{DynamicRenderElements, RenderError, Space},
};
use smithay::utils::{Coordinate, Logical, Rectangle, Size};
use smithay::wayland::output::Output;

use slog::Logger;

use crate::drawing::CLEAR_COLOR;
use crate::shell::FullscreenSurface;

pub fn render_output<R>(
    output: &Output,
    space: &mut Space,
    renderer: &mut R,
    age: usize,
    elements: &[DynamicRenderElements<R>],
    log: &Logger,
) -> Result<Option<Vec<Rectangle<i32, Logical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + 'static,
    R::Frame: 'static,
    R::TextureId: 'static,
    R::Error: 'static,
{
    let window = match output
        .user_data()
        .get::<FullscreenSurface>()
        .and_then(|f| f.get())
    {
        Some(w) => w,
        None => return space.render_output(renderer, output, age, CLEAR_COLOR, elements),
    };

    let transform = output.current_transform().into();
    let mode = output.current_mode().unwrap();
    let scale = space.output_scale(output).unwrap();
    let output_geo = space.output_geometry(output).unwrap_or_default();

    fn damage_rect<T: Coordinate + Default, U>(
        size: impl Into<Size<T, U>>,
    ) -> [Rectangle<T, U>; 1] {
        [Rectangle::from_loc_and_size(
            (T::default(), T::default()),
            size.into(),
        )]
    }

    let rendering = |renderer: &mut _, frame: &mut R::Frame| -> Result<_, R::Error> {
        let mut damage = window.accumulated_damage(None);
        frame.clear(CLEAR_COLOR, &damage_rect(mode.size))?;

        let dmg = damage_rect(mode.size.to_f64().to_logical(scale).to_i32_round());
        draw_window(renderer, frame, &window, scale, (0, 0), &dmg, log)?;

        for elem in elements {
            let elem_geo = elem.geometry();
            let location = elem_geo.loc - output_geo.loc;
            let elem_damage = elem.accumulated_damage(None);

            let dmg = damage_rect(elem_geo.size);
            elem.draw(renderer, frame, scale, location, &dmg, log)?;

            for mut rect in elem_damage {
                rect.loc += elem_geo.loc;
                damage.push(rect);
            }
        }

        Ok(damage)
    };

    let res = renderer.render(mode.size, transform, rendering);

    match res {
        Ok(Ok(x)) => Ok(Some(x)),
        Ok(Err(e)) | Err(e) => Err(RenderError::Rendering(e)),
    }
}
