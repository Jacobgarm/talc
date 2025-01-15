use crate::{ast::Exp, context::Context, eval::EvalResult, linalg};

use super::derivative;

pub fn gradient(exp: &Exp, diff_exps: &[Exp], ctx: &Context) -> EvalResult<Exp> {
    let mut derivatives = Vec::with_capacity(diff_exps.len());
    for diff_exp in diff_exps {
        let deri = derivative(exp, diff_exp, ctx)?;
        derivatives.push(deri);
    }
    Ok(linalg::Matrix::from_col(derivatives).into())
}

pub fn hessian(exp: &Exp, diff_exps: &[Exp], ctx: &Context) -> EvalResult<Exp> {
    let mut rows = Vec::with_capacity(diff_exps.len());
    for exp1 in diff_exps {
        let mut row = Vec::with_capacity(diff_exps.len());
        for exp2 in diff_exps {
            let deri1 = derivative(exp, exp2, ctx)?;
            let deri2 = derivative(&deri1, exp1, ctx)?;

            row.push(deri2);
        }
        rows.push(row);
    }
    Ok(linalg::Matrix::try_from_rows(rows).unwrap().into())
}

pub fn hessian_symmetric(exp: &Exp, diff_exps: &[Exp], ctx: &Context) -> EvalResult<Exp> {
    let n = diff_exps.len();
    let mut rows = vec![vec![Exp::ZERO; n]; n];
    for (i, exp1) in diff_exps.iter().enumerate() {
        for (j, exp2) in diff_exps.iter().take(i + 1).enumerate() {
            let deri1 = derivative(exp, exp1, ctx)?;
            let deri2 = derivative(&deri1, exp2, ctx)?;
            rows[i][j] = deri2.clone();
            rows[j][i] = deri2;
        }
    }

    Ok(linalg::Matrix::try_from_rows(rows).unwrap().into())
}
