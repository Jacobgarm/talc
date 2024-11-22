use crate::{
    ast::{DyadicOp, Exp, UnaryOp},
    context::Context,
    linalg,
    typing::ExpType,
    utils::try_int_to_signed,
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

pub fn eval_pow(left: Exp, right: Exp, ctx: &Context) -> EvalResult<Exp> {
    let Exp::Number(pow) = right else {
        return Ok(Exp::Dyadic {
            op: DyadicOp::Pow,
            left: left.into(),
            right: right.into(),
        });
    };

    if pow.is_exact_one() {
        return Ok(left);
    }

    if pow.is_exact_zero() {
        if let Exp::Matrix(mat) = left {
            return if !mat.is_square() {
                Err(EvalError::NonSquareMatrixPower { size: mat.size() })
            } else {
                Ok(linalg::Matrix::identity(mat.height()).into())
            };
        }

        if matches!(left.infer_type(ctx), ExpType::Unknown | ExpType::Numeric) {
            return Ok(Exp::ONE);
        }
    }

    if let Ok(pow_int) = malachite::Integer::try_from(pow.clone()) {
        match left.clone() {
            Exp::Number(num) => {
                if pow_int < 0 && num.is_zero() {
                    return Err(EvalError::DivisionByZero);
                }
                if let Some(pow_i64) = try_int_to_signed(&pow_int) {
                    return Ok(num.powi(pow_i64).into());
                }
            }
            _ => (),
        }
    }

    Ok(Exp::Dyadic {
        op: DyadicOp::Pow,
        left: left.into(),
        right: Exp::Number(pow).into(),
    })
}
