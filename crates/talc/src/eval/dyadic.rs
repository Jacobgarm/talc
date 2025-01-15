use malachite::num::{conversion::traits::IsInteger, float::NiceFloat};
use talc_utils::try_one_to_zero_index;

use crate::{
    ast::{AssocOp, ComplexNum, DyadicOp, Exp, RealNum, UnaryOp},
    context::Context,
    linalg::{self, Matrix},
    typing::ExpType,
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
    if let Exp::Real(ref a) = left
        && let Exp::Real(ref b) = right
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
    match right {
        Exp::Real(num) => real_pow(left, num, ctx),
        Exp::Complex(num) => {
            let real_exp = real_pow(left.clone(), num.real, ctx)?;
            let imag_exp = imag_pow(left, num.imag, ctx)?;
            let numeric = real_exp.is_number() && imag_exp.is_number();
            let exp = if let Exp::Real(num) = &real_exp
                && num.is_one()
            {
                imag_exp
            } else if let Exp::Real(num) = &imag_exp
                && num.is_one()
            {
                real_exp
            } else {
                Exp::assoc_combine(AssocOp::Mul, real_exp, imag_exp)
            };
            if numeric { exp.eval(ctx) } else { Ok(exp) }
        }
        right => Ok(Exp::Dyadic {
            op: DyadicOp::Pow,
            left: left.into(),
            right: right.into(),
        }),
    }
}

fn real_pow(base: Exp, pow: RealNum, ctx: &Context) -> EvalResult<Exp> {
    if pow.is_exact_one() {
        return Ok(base);
    }

    if pow.is_exact_zero() {
        if let Exp::Matrix(mat) = base {
            return if mat.is_square() {
                Ok(linalg::Matrix::identity(mat.height()).into())
            } else {
                Err(EvalError::NonSquareMatrixPower { size: mat.size() })
            };
        }

        if matches!(base.infer_type(ctx), ExpType::Unknown | ExpType::Numeric) {
            return Ok(Exp::ONE);
        }
    }

    if let Ok(pow_int) = malachite::Integer::try_from(pow.clone()) {
        #[allow(clippy::single_match)]
        match base.clone() {
            Exp::Real(num) => {
                if pow_int < 0 && num.is_zero() {
                    return Err(EvalError::DivisionByZero);
                }
                if let Ok(pow_i64) = i64::try_from(&pow_int) {
                    return Ok(num.powi(pow_i64).into());
                }
            }
            _ => (),
        }
    }

    if let (Exp::Real(RealNum::Small(a)), RealNum::Small(b)) = (base.clone(), pow.clone())
        && (a.0 > 0.0 || b.0.is_integer())
    {
        return Ok(RealNum::Small(NiceFloat(a.0.powf(b.0))).into());
    }

    Ok(Exp::Dyadic {
        op: DyadicOp::Pow,
        left: base.into(),
        right: Exp::Real(pow).into(),
    })
}

fn imag_pow(base: Exp, pow: RealNum, ctx: &Context) -> EvalResult<Exp> {
    if let Exp::Var { name } = &base
        && name == "e"
        && ctx.is_default_var(name)
    {
        let cis = crate::parse::parse("cos(x)+i*sin(x)").unwrap();
        let exp = cis.replace(
            &Exp::Var {
                name: "x".to_owned(),
            },
            &pow.clone().into(),
        );
        return exp.eval(ctx);
    }

    Ok(Exp::Dyadic {
        op: DyadicOp::Pow,
        left: base.into(),
        right: Exp::Complex(ComplexNum::from_imag(pow)).into(),
    })
}

pub fn get_numeric_index(index: Exp) -> EvalResult<Option<usize>> {
    match index {
        Exp::Real(ref num) => match usize::try_from(num.clone()) {
            Ok(idx) => Ok(Some(idx)),
            Err(_) => Err(EvalError::NonNaturalIndex { index }),
        },
        Exp::Bool(_)
        | Exp::Inf
        | Exp::Complex(_)
        | Exp::RelationChain { .. }
        | Exp::Tuple(_)
        | Exp::Matrix(_) => Err(EvalError::NonNaturalIndex { index }),
        _ => Ok(None),
    }
}

pub fn eval_index(exp: Exp, index: Exp, _ctx: &Context) -> EvalResult<Exp> {
    let exp_res = match exp {
        Exp::Matrix(_) | Exp::Tuple(_) => Some(exp.clone()),
        Exp::Real(_) | Exp::Complex(_) | Exp::Bool(_) | Exp::Inf | Exp::RelationChain { .. } => {
            return Err(EvalError::NonIndexableExp { exp });
        }
        _ => None,
    };
    let index_res = match index.clone() {
        Exp::Real(num) => match usize::try_from(num) {
            Ok(idx) => Some(vec![idx]),
            Err(_) => return Err(EvalError::NonNaturalIndex { index }),
        },
        Exp::Tuple(entries) => entries.into_iter().map(get_numeric_index).try_collect()?,
        Exp::Bool(_) | Exp::Inf | Exp::Complex(_) | Exp::RelationChain { .. } | Exp::Matrix(_) => {
            return Err(EvalError::NonNaturalIndex { index });
        }
        _ => None,
    };

    if let (Some(obj), Some(one_index)) = (exp_res, index_res) {
        let Some(zero_index) = try_one_to_zero_index(&one_index) else {
            return Err(EvalError::NonNaturalIndex {
                index: index.clone(),
            });
        };

        match obj {
            Exp::Tuple(mut entries) => {
                if zero_index.len() != 1 || zero_index[0] >= entries.len() {
                    Err(EvalError::IndexOutOfBounds {
                        size: vec![entries.len()],
                        index: one_index,
                    })
                } else {
                    Ok(entries.remove(zero_index[0]))
                }
            }
            Exp::Matrix(mat) => {
                let err = EvalError::IndexOutOfBounds {
                    size: vec![mat.height(), mat.width()],
                    index: one_index,
                };
                match zero_index[..] {
                    [i, j] => mat.get((i, j)).cloned().ok_or(err),
                    [i] => {
                        if mat.width() == 1 {
                            mat.get((i, 0)).cloned().ok_or(err)
                        } else {
                            Err(err)
                        }
                    }
                    _ => Err(err),
                }
            }
            _ => unreachable!(),
        }
    } else {
        Ok(Exp::Dyadic {
            op: DyadicOp::Index,
            left: exp.into(),
            right: index.into(),
        })
    }
}

pub fn eval_dot_product(left: Exp, right: Exp, ctx: &Context) -> EvalResult<Exp> {
    if let (Exp::Matrix(mat1), Exp::Matrix(mat2)) = (&left, &right) {
        if mat1.size() == mat2.size() {
            mat1.clone().scalar_product(mat2.clone()).eval(ctx)
        } else {
            Err(EvalError::MatrixSizesIncompatibleDot {
                left_size: mat1.size(),
                right_size: mat2.size(),
            })
        }
    } else {
        Ok(Exp::Dyadic {
            op: DyadicOp::DotProd,
            left: left.into(),
            right: right.into(),
        })
    }
}

pub fn eval_cross_product(left: Exp, right: Exp, ctx: &Context) -> EvalResult<Exp> {
    if let (Exp::Matrix(vec1), Exp::Matrix(vec2)) = (&left, &right) {
        let err = Err(EvalError::MatrixSizesIncompatibleCross {
            left_size: vec1.size(),
            right_size: vec2.size(),
        });

        if vec1.width() == 1 && vec2.width() == 1 && vec1.height() == vec2.height() {
            match vec1.height() {
                0 => Ok(linalg::Matrix::filled(Exp::ZERO, (0, 0)).into()),
                1 => Ok(linalg::Matrix::filled(Exp::ZERO, (1, 1)).into()),
                3 => cross_product_3d(&vec1.cols_ref()[0], &vec2.cols_ref()[0]).eval(ctx),
                7 => cross_product_7d(&vec1.cols_ref()[0], &vec2.cols_ref()[0]).eval(ctx),
                _ => err,
            }
        } else {
            err
        }
    } else {
        Ok(Exp::Dyadic {
            op: DyadicOp::CrossProd,
            left: left.into(),
            right: right.into(),
        })
    }
}

fn cross_product_3d(a: &[&Exp], b: &[&Exp]) -> Exp {
    let v1 = a[1].clone() * b[2].clone() - a[2].clone() * b[1].clone();
    let v2 = a[2].clone() * b[0].clone() - a[0].clone() * b[2].clone();
    let v3 = a[0].clone() * b[1].clone() - a[1].clone() * b[0].clone();
    Matrix::from_col(vec![v1, v2, v3]).into()
}

fn cross_product_7d(a: &[&Exp], b: &[&Exp]) -> Exp {
    unimplemented!()
}
