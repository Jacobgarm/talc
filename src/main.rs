use talc::context::Context;

mod repl;

fn main() {
    let context = Context::default();
    arbitrary_code(context.clone());
    repl::repl(context);
}

fn arbitrary_code(context: Context) {
    let a = malachite_float::Float::from_unsigned_prec(1u32, 2);
    let b = malachite_float::Float::from_unsigned_prec(10u32, 2);
    dbg!(&a, &b);
    println!("{}", (&a.0 / &b.0) * &b.0 == a.0);
    //dbg!(parse("-x"));
    //dbg!(ast::ProcedureType::from_str("approx").unwrap().to_string());
}
