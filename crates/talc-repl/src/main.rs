#![feature(let_chains)]

use std::path::Path;

use talc::{compute::RealFunction, context::Context, parse::parse};
use talc_plot::Plot2DOptions;

mod repl;

fn main() {
    let ctx = Context::default();
    //arbitrary_code(ctx.clone());
    repl::repl(ctx);
}

#[allow(unused_variables, clippy::needless_pass_by_value)]
fn arbitrary_code(ctx: Context) {
    let f = RealFunction::new(&parse("sin(x)").unwrap(), vec!["x".to_owned()], &ctx).unwrap();
    talc_plot::plot_2d(Path::new("plot.svg"), vec![f], &Plot2DOptions::default()).unwrap();
    talc_plot::draw().unwrap();
    open::that("white.svg").unwrap();
}
