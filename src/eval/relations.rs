use itertools::Itertools;

use crate::{
    ast::{Exp, Relation},
    context::Context,
    typing::ExpType,
};

use super::EvalResult;

pub fn eval_chain(rels: Vec<Relation>, terms: Vec<Exp>, ctx: &Context) -> EvalResult<Exp> {
    let mut all_true = true;

    for (pair, rel) in terms.windows(2).zip_eq(rels.iter()) {
        let left = &pair[0];
        let right = &pair[1];
        let res = match rel {
            Relation::Eq => check_eq(left, right, ctx),
            _ => None,
        };

        match res {
            Some(true) => continue,
            Some(false) => return Ok(Exp::Bool(false)),
            None => all_true = false,
        }
    }

    Ok(if all_true {
        Exp::Bool(true)
    } else {
        Exp::RelationChain { rels, terms }
    })
}

pub fn check_eq(left: &Exp, right: &Exp, ctx: &Context) -> Option<bool> {
    if left == right {
        Some(true)
    } else if left.infer_type(ctx) != ExpType::Unknown
        && right.infer_type(ctx) != ExpType::Unknown
        && left.infer_type(ctx) != right.infer_type(ctx)
    {
        Some(false)
    } else if left.is_simple() && right.is_simple() && left != right {
        Some(false)
    } else if let Exp::Matrix(a) = left
        && let Exp::Matrix(b) = right
    {
        if a.size() != b.size() {
            Some(false)
        } else if a
            .iter_by_rows()
            .zip_eq(b.iter_by_rows())
            .any(|(exp1, exp2)| check_eq(exp1, exp2, ctx) == Some(false))
        {
            Some(false)
        } else {
            None
        }
    } else {
        None
    }
}
