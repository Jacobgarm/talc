use crate::ast::{AssocOp, Exp, Numeric, ProcedureKind};
use crate::context::Context;
use crate::linalg;

mod fold;

#[derive(Debug, Clone)]
pub enum EvalError {
    DivisionByZero,
    ProcedureError(String),
    MatrixScalarSum,
    MatrixSizesIncompatibleAdd {
        left_size: (usize, usize),
        right_size: (usize, usize),
    },
    MatrixSizesIncompatibleMul {
        left_size: (usize, usize),
        right_size: (usize, usize),
    },

    Unimplemented,
}

type EvalResult<T> = Result<T, EvalError>;

impl Exp {
    pub fn eval(&self, ctx: &Context) -> EvalResult<Exp> {
        use Exp::*;

        let exp = if let Procedure { kind, .. } = self {
            if kind.eagerly_eval_args() {
                self.try_map(|exp| exp.eval(ctx))?
            } else {
                self.clone()
            }
        } else {
            self.try_map(|exp| exp.eval(ctx))?
        };
        Ok(match exp {
            Var { ref name } => {
                if let Some(info) = ctx.get_var(name) {
                    info.exp.clone()
                } else {
                    exp
                }
            }
            Pool { op, terms } => eval_pool(op, terms, ctx)?,
            Procedure { kind, args } => eval_procedure(kind, &args, ctx)?,
            _ => exp.clone(),
        })
    }
}

fn eval_pool(op: AssocOp, terms: Vec<Exp>, ctx: &Context) -> EvalResult<Exp> {
    let mut new_terms = match op {
        AssocOp::Add => fold::add_fold(terms, ctx)?,
        AssocOp::Mul => fold::mul_fold(terms, ctx)?,
        _ => unimplemented!(),
    };
    Ok(if new_terms.len() == 1 {
        new_terms.pop().unwrap()
    } else {
        Exp::Pool {
            op,
            terms: new_terms,
        }
    })
}

fn eval_procedure(kind: ProcedureKind, args: &[Vec<Exp>], ctx: &Context) -> EvalResult<Exp> {
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
            if let Number(Numeric::Integer(int)) = &args[0][0] {
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

        _ => return Err(EvalError::Unimplemented),
    };

    Ok(result.unwrap_or_else(|| Exp::Procedure {
        kind,
        args: args.to_vec(),
    }))
}
