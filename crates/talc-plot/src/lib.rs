use std::path::Path;

use plotters::prelude::*;
use talc::compute::RealFunction;

pub struct Axis {
    name: String,
    horizontal: bool,
}

impl Axis {
    fn new(name: String, horizontal: bool) -> Self {
        Self { name, horizontal }
    }
}

pub struct Plot2DOptions {
    axes: Vec<Axis>,
}

impl Default for Plot2DOptions {
    fn default() -> Self {
        Self {
            axes: vec![
                Axis::new("x".to_owned(), true),
                Axis::new("y".to_owned(), false),
            ],
        }
    }
}

pub fn plot_2d(
    path: &Path,
    mut funcs: Vec<RealFunction>,
    opts: &Plot2DOptions,
) -> Result<(), Box<dyn std::error::Error>> {
    let root = SVGBackend::new(&path, (640, 480)).into_drawing_area();

    root.fill(&WHITE)?;

    let mut chart = ChartBuilder::on(&root)
        .caption("Talc plot", ("sans-serif", 50).into_font())
        .margin(5)
        .x_label_area_size(30)
        .y_label_area_size(30)
        .build_cartesian_2d(-3f32..3f32, -1f32..1f32)?;
    chart.configure_mesh().draw()?;

    for func in &mut funcs {
        chart
            .draw_series(LineSeries::new(
                (-1500..=1500)
                    .map(|x| x as f32 / 500.0)
                    .map(|x| (x, x.sin())),
                //.map(|x| (x, func.eval_point(&[f64::from(x)]).unwrap() as f32)),
                &RED,
            ))?
            .label(format!("y = {}", func.exp))
            .legend(|(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], RED));
    }

    root.present()?;
    Ok(())
}

pub fn draw() -> Result<(), Box<dyn std::error::Error>> {
    let root = SVGBackend::new("white.svg", (1920, 1080)).into_drawing_area();
    root.fill(&WHITE)?;
    let mut chart = ChartBuilder::on(&root)
        .caption("y=x^2", ("sans-serif", 50).into_font())
        .margin(5)
        .x_label_area_size(30)
        .y_label_area_size(30)
        .build_cartesian_2d(-1f32..1f32, -0.1f32..1f32)?;

    chart.configure_mesh().draw()?;

    chart
        .draw_series(LineSeries::new(
            (-5000..=5000)
                .map(|x| x as f32 / 5000.0)
                .map(|x| (x, x * x)),
            &RED,
        ))?
        .label("y = x^2")
        .legend(|(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], RED));

    chart
        .configure_series_labels()
        .background_style(WHITE.mix(0.8))
        .border_style(BLACK)
        .draw()?;

    root.present()?;
    println!("Presented");
    Ok(())
}
