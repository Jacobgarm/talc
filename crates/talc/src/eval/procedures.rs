use super::{EvalError, EvalResult, Exp};

use crate::ast::ProcedureKind;
use crate::ast::RealNum;
use crate::context::ApproxLevel;
use crate::context::Context;
use crate::linalg;

pub fn eval_procedure(kind: ProcedureKind, args: Vec<Vec<Exp>>, ctx: &Context) -> EvalResult<Exp> {
    use Exp::*;
    use ProcedureKind as PK;
    let result = match kind {
        PK::ColumnVector => Some(Matrix(linalg::Matrix::from_col(args[0].to_vec()))),
        PK::RowVector => Some(Matrix(linalg::Matrix::from_row(args[0].to_vec()))),
        PK::Matrix => Some(Matrix(
            linalg::Matrix::try_from_rows(args.to_vec()).expect("jagged matrix"),
        )),
        PK::DiagonalMatrix => Some(Matrix(linalg::Matrix::diagonal(args[0].to_vec()))),
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
            let mut new_context = ctx.clone();
            new_context.approx_level = if args[0].len() == 2 {
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
                ApproxLevel::BigFloat(prec)
            } else {
                ApproxLevel::SmallFloat
            };
            Some(args[0][0].eval(&new_context)?)
        }

        _ => return Err(EvalError::Unimplemented),
    };

    Ok(result.unwrap_or_else(|| Exp::Procedure {
        kind,
        args: args.to_vec(),
    }))
}
