use itertools::Itertools;
use malachite::{Integer, Natural, num::arithmetic::traits::Factorial};

use crate::{
    ast::{AssocOp, Exp, RealNum, UnaryOp},
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

pub fn eval_abs(exp: Exp, _ctx: &Context) -> EvalResult<Exp> {
    match exp {
        Exp::Real(num) => Ok(num.abs().into()),
        _ => Ok(Exp::Unary {
            op: UnaryOp::Factorial,
            operand: exp.into(),
        }),
    }
}

pub fn eval_norm(exp: Exp, ctx: &Context) -> EvalResult<Exp> {
    match exp {
        Exp::Matrix(mat) => {
            let terms = mat
                .into_iter_by_rows()
                .map(|term| term.pow(Exp::TWO))
                .collect_vec();
            let sum_squares = Exp::Pool {
                op: AssocOp::Add,
                terms,
            };
            sum_squares.pow(Exp::ONE_HALF).eval(ctx)
        }
        _ => Ok(Exp::Unary {
            op: UnaryOp::Factorial,
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
