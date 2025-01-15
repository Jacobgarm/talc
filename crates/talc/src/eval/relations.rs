use itertools::Itertools;

use crate::{
    ast::{Exp, Relation},
    context::Context,
    typing::ExpType,
};

use super::{EvalError, EvalResult};

#[allow(clippy::needless_pass_by_value)]
pub fn eval_chain(rels: Vec<Relation>, terms: Vec<Exp>, ctx: &Context) -> EvalResult<Exp> {
    if terms.len() < 2 {
        return Err(EvalError::RelationChainTooShort { len: terms.len() });
    }
    let mut all_true = true;
    let mut new_terms = Vec::new();
    let mut new_rels = Vec::new();

    for (pair, rel) in terms.windows(2).zip_eq(rels.into_iter()) {
        let left = &pair[0];
        let right = &pair[1];
        let res = match rel {
            Relation::Eq => check_eq(left, right, ctx),
            Relation::Neq => check_eq(left, right, ctx).map(|b| !b),
            Relation::Lt => check_lt(left, right, ctx),
            Relation::Geq => check_lt(left, right, ctx).map(|b| !b),
            Relation::Gt => check_gt(left, right, ctx),
            Relation::Leq => check_gt(left, right, ctx).map(|b| !b),
            _ => None,
        };

        match res {
            Some(true) => continue,
            Some(false) => return Ok(Exp::Bool(false)),
            None => all_true = false,
        }

        if !all_true {
            new_rels.push(rel);
            new_terms.push(left.clone());
        }
    }

    new_terms.push(terms.last().unwrap().clone());

    Ok(if all_true {
        Exp::Bool(true)
    } else {
        Exp::RelationChain {
            rels: new_rels,
            terms: new_terms,
        }
    })
}

pub fn check_eq(left: &Exp, right: &Exp, ctx: &Context) -> Option<bool> {
    #[allow(clippy::if_same_then_else)]
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
        if a.size() == b.size() {
            let mut all_true = true;
            for (e1, e2) in a.iter_by_rows().zip_eq(b.iter_by_rows()) {
                match check_eq(e1, e2, ctx) {
                    Some(false) => return Some(false),
                    Some(true) => (),
                    None => all_true = false,
                }
            }
            if all_true {
                Some(true)
            } else {
                None
            }
        } else {
            Some(false)
        }
    } else {
        None
    }
}

pub fn check_lt(left: &Exp, right: &Exp, _ctx: &Context) -> Option<bool> {
    if left == right {
        return Some(false);
    }
    match (left, right) {
        (Exp::Real(a), Exp::Real(b)) => Some(a < b),
        _ => None,
    }
}

pub fn check_gt(left: &Exp, right: &Exp, _ctx: &Context) -> Option<bool> {
    if left == right {
        return Some(false);
    }
    match (left, right) {
        (Exp::Real(a), Exp::Real(b)) => Some(a > b),
        _ => None,
    }
}
