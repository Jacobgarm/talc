use itertools::Itertools;
use malachite::num::float::NiceFloat;

use crate::{
    ast::{Exp, Numeric},
    context::{ApproxLevel, Context},
};

use super::{EvalError, EvalResult};

pub fn eval_function(
    name: String,
    primes: Vec<usize>,
    args: Vec<Exp>,
    ctx: &Context,
) -> EvalResult<Exp> {
    let Some(info) = ctx.get_function(&name) else {
        return Ok(Exp::Function { name, primes, args });
    };

    if info.params.len() != args.len() {
        return Err(EvalError::FunctionWrongArgCount {
            expected: info.params.len(),
            got: args.len(),
        });
    }

    if !primes.is_empty() {
        todo!()
    }

    if ctx.approx_level == ApproxLevel::SmallFloat
        && let Some(float_func) = info.float_func
    {
        if let Exp::Number(Numeric::Small(num)) = args[0] {
            let val = float_func(num.0);
            return Ok(Numeric::Small(NiceFloat(val)).into());
        }
    }

    if let Some(func_exp) = info.exp.clone() {
        let mut func_context = ctx.clone();
        for (param, arg) in info.params.iter().zip_eq(args) {
            func_context.set_var(param.to_owned(), arg);
        }
        let new_exp = func_exp.eval(&func_context)?;
        return Ok(new_exp);
    }

    Ok(Exp::Function { name, primes, args })
}
