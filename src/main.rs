use talc::context::Context;

mod repl;

fn main() {
    let context = Context::default();
    arbitrary_code(context.clone());
    repl::repl(context);
}

#[allow(unused_variables)]
fn arbitrary_code(context: Context) {}
