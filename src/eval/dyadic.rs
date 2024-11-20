use crate::{
    ast::{DyadicOp, Exp, UnaryOp},
    context::Context,
};

use super::{EvalError, EvalResult};

pub fn eval_equiv(left: Exp, right: Exp, _ctx: &Context) -> EvalResult<Exp> {
    if left == right {
        Ok(Exp::Bool(true))
    } else if let Exp::Bool(a) = left
        && let Exp::Bool(b) = right
    {
        Ok(Exp::Bool(a == b))
    } else if let Exp::Unary {
        op: UnaryOp::Not,
        ref operand,
    } = left
        && **operand == right
    {
        Ok(Exp::Bool(false))
    } else if let Exp::Unary {
        op: UnaryOp::Not,
        ref operand,
    } = right
        && **operand == left
    {
        Ok(Exp::Bool(false))
    } else {
        Ok(Exp::Dyadic {
            op: DyadicOp::LogicEquiv,
            left: left.into(),
            right: right.into(),
        })
    }
}

pub fn eval_implies(left: Exp, right: Exp, _ctx: &Context) -> EvalResult<Exp> {
    if left == right {
        Ok(Exp::Bool(true))
    } else if let Exp::Bool(a) = left
        && let Exp::Bool(b) = right
    {
        Ok(Exp::Bool(!a || b))
    } else {
        Ok(Exp::Dyadic {
            op: DyadicOp::LogicImplies,
            left: left.into(),
            right: right.into(),
        })
    }
}

pub fn eval_mod(left: Exp, right: Exp, _ctx: &Context) -> EvalResult<Exp> {
    if let Exp::Number(ref a) = left
        && let Exp::Number(ref b) = right
    {
        if b.is_zero() {
            Err(EvalError::ModuloByZero)
        } else {
            Ok(a.euclid_mod(b).into())
        }
    } else {
        Ok(Exp::Dyadic {
            op: DyadicOp::Mod,
            left: left.into(),
            right: right.into(),
        })
    }
}
