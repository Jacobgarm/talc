use malachite::{num::arithmetic::traits::Factorial, Integer, Natural};

use crate::{
    ast::{Exp, RealNum, UnaryOp},
    context::Context,
};

use super::EvalResult;

pub fn eval_not(exp: Exp, _ctx: &Context) -> EvalResult<Exp> {
    match exp {
        Exp::Bool(b) => Ok(Exp::Bool(!b)),
        Exp::Unary {
            op: UnaryOp::Not,
            operand,
        } => Ok(*operand),
        _ => Ok(Exp::Unary {
            op: UnaryOp::Not,
            operand: exp.into(),
        }),
    }
}

pub fn eval_factorial(exp: Exp, _ctx: &Context) -> EvalResult<Exp> {
    match exp {
        Exp::Real(RealNum::Integer(int)) if let Ok(unsigned) = u64::try_from(&int) => {
            let fac = Natural::factorial(unsigned);
            Ok(RealNum::Integer(Integer::from(fac)).into())
        }
        _ => Ok(Exp::Unary {
            op: UnaryOp::Factorial,
            operand: exp.into(),
        }),
    }
}
