use std::collections::HashMap;

use itertools::Itertools;
use malachite::num::float::NiceFloat;

use crate::{
    ast::{Exp, RealNum},
    calculus::expand_primed_function,
    context::{ApproxLevel, Context},
};

use super::{EvalError, EvalResult};

pub fn eval_function(
    name: String,
    primes: Vec<usize>,
    args: Vec<Exp>,
    ctx: &Context,
) -> EvalResult<Exp> {
    let Some(info) = ctx.get_func(&name) else {
        return Ok(Exp::Function { name, primes, args });
    };

    if info.params.len() != args.len() {
        return Err(EvalError::FunctionWrongArgCount {
            name,
            expected: info.params.len(),
            got: args.len(),
        });
    }
    if primes.is_empty()
        && ctx.approx_level == ApproxLevel::SmallFloat
        && let Some(float_func) = info.float_func
    {
        if let Exp::Real(RealNum::Small(num)) = args[0] {
            let val = float_func(num.0);
            return Ok(RealNum::Small(NiceFloat(val)).into());
        }
    }

    let func_exp = if !primes.is_empty() {
        expand_primed_function(&name, &primes, ctx)?.unwrap()
    } else if let Some(func_exp) = info.exp.clone() {
        func_exp
    } else {
        return Ok(Exp::Function { name, primes, args });
    };

    //let mut func_context = ctx.clone();
    //for (param, arg) in info.params.iter().zip_eq(args) {
    //    func_context.set_var(param.to_owned(), arg);
    //}
    //let new_exp = func_exp.eval(&func_context)?;

    let mut subs = HashMap::new();
    for (param, arg) in info.params.iter().zip_eq(args) {
        subs.insert(
            Exp::Var {
                name: param.clone(),
            },
            arg,
        );
    }
    let new_exp = func_exp.substitute(&subs);

    new_exp.eval(ctx)
}
