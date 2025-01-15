use itertools::Itertools;

use super::{EvalError, EvalResult, Exp};

use crate::ast::ProcedureKind;
use crate::ast::RealNum;
use crate::calculus;
use crate::context::Context;
use crate::linalg;
use crate::solve;

#[allow(clippy::needless_pass_by_value)]
pub fn eval_procedure(kind: ProcedureKind, args: Vec<Vec<Exp>>, ctx: &Context) -> EvalResult<Exp> {
    use Exp::*;
    use ProcedureKind as PK;
    let result = match kind {
        PK::ColumnVector => Some(Matrix(linalg::Matrix::from_col(args[0].clone()))),
        PK::RowVector => Some(Matrix(linalg::Matrix::from_row(args[0].clone()))),
        PK::Matrix => Some(Matrix(
            linalg::Matrix::try_from_rows(args.clone()).expect("jagged matrix"),
        )),
        PK::DiagonalMatrix => Some(Matrix(linalg::Matrix::diagonal(args[0].clone()))),
        PK::IdentityMatrix => {
            if let Real(RealNum::Integer(int)) = &args[0][0] {
                if int < &0 {
                    return Err(EvalError::ProcedureError(
                        "cannot have negative sized identity matrix".to_owned(),
                    ));
                }
                let size = usize::try_from(int).expect("too large identity matrix");
                Some(Exp::Matrix(linalg::Matrix::identity(size)))
            } else {
                None
            }
        }
        PK::EvalDefaultContext => {
            Some(crate::context::DEFAULT_CONTEXT.with_borrow(|def_ctx| args[0][0].eval(def_ctx))?)
        }
        PK::Approximate => {
            let prec = if args[0].len() == 2 {
                let Real(RealNum::Integer(ref prec_int)) = args[0][1] else {
                    return Err(EvalError::ProcedureError(format!(
                        "invalid precision {}",
                        args[0][0]
                    )));
                };

                let Ok(prec) = u64::try_from(prec_int) else {
                    return Err(EvalError::ProcedureError(format!(
                        "invalid precision {}",
                        args[0][0]
                    )));
                };
                Some(prec)
            } else {
                None
            };
            Some(args[0][0].approximate(prec, ctx)?)
        }
        PK::Isolate => solve::isolate(args[0][0].clone(), args[0][1].clone())?,
        PK::Derivative => {
            let diff_exp = &args[0][1];
            let mut new_ctx = ctx.clone();
            for var in diff_exp.vars() {
                new_ctx.del_var(&var);
            }
            let exp = args[0][0].eval(ctx)?;
            let deri = calculus::derivative(&exp, diff_exp, &new_ctx)?;
            Some(deri.eval(ctx)?)
        }
        PK::Gradient => {
            let diff_vars = if let Some(sub_args) = args.get(1) {
                sub_args.clone()
            } else {
                let mut vars = args[0][0]
                    .vars()
                    .into_iter()
                    .filter(|var| ctx.get_var(var).is_none())
                    .collect_vec();
                vars.sort();
                vars.into_iter()
                    .map(|name| Exp::Var { name: name.clone() })
                    .collect_vec()
            };
            let gradient = calculus::gradient(&args[0][0], &diff_vars, ctx)?;
            Some(gradient.eval(ctx)?)
        }
        PK::Hessian => {
            let diff_vars = if let Some(sub_args) = args.get(1) {
                sub_args.clone()
            } else {
                let mut vars = args[0][0]
                    .vars()
                    .into_iter()
                    .filter(|var| ctx.get_var(var).is_none())
                    .collect_vec();
                vars.sort();
                vars.into_iter()
                    .map(|name| Exp::Var {
                        name: name.to_owned(),
                    })
                    .collect_vec()
            };
            let hessian = calculus::hessian_symmetric(&args[0][0], &diff_vars, ctx)?;
            Some(hessian.eval(ctx)?)
        }

        _ => return Err(EvalError::Unimplemented),
    };

    Ok(result.unwrap_or_else(|| Exp::Procedure {
        kind,
        args: args.clone(),
    }))
}
