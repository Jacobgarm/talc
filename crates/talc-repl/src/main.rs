#![feature(let_chains)]

use std::env;
use std::path::Path;

use exec::ExecContext;
use itertools::Itertools;
use talc::context::Context;

mod common;
mod exec;
mod repl;

fn main() -> std::io::Result<()> {
    let args = env::args().collect_vec();
    let mut math_ctx = Context::default();
    if args.len() == 1 {
        arbitrary_code(math_ctx.clone());
        repl::repl(&mut math_ctx);
        Ok(())
    } else {
        let path = Path::new(&args[1]);
        exec::exec_file(path, &mut ExecContext::default(), &mut math_ctx)
    }
}

#[allow(unused_variables, clippy::needless_pass_by_value)]
fn arbitrary_code(ctx: Context) {
    //let f = RealFunction::new(&parse("sin(x)").unwrap(), vec!["x".to_owned()], &ctx).unwrap();
    //talc_plot::plot_2d(Path::new("plot.svg"), vec![f], &Plot2DOptions::default()).unwrap();
    //talc_plot::draw().unwrap();
    //open::that("white.svg").unwrap();
}
