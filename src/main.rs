use talc::context::Context;

mod repl;

fn main() {
    let context = Context::default();
    arbitrary_code(context.clone());
    repl::repl(context);
}

fn arbitrary_code(context: Context) {

    //dbg!(parse("-x"));
    //dbg!(ast::ProcedureType::from_str("approx").unwrap().to_string());
}
