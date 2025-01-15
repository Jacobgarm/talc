use crate::{
    ast::{Exp, Relation},
    eval::{EvalError, EvalResult},
};

pub fn isolate(exp: Exp, qty: Exp) -> EvalResult<Option<Exp>> {
    if !exp.contains(&qty) {
        return Err(EvalError::ProcedureError(
            "cannot isolate for expression not present".to_owned(),
        ));
    }
    let Exp::RelationChain { rels, mut terms } = exp else {
        return Err(EvalError::ProcedureError(
            "cannot isolate in expression that is not a relation".to_owned(),
        ));
    };
    if rels.len() != 1 {
        return Err(EvalError::ProcedureError(
            "cannot isolate in chained relations".to_owned(),
        ));
    }

    let mut right = terms.pop().unwrap();
    let mut left = terms.pop().unwrap();

    if !left.contains(&qty) {
        (left, right) = (right, left);
    };

    let res = match rels[0] {
        Relation::Eq => isolate_eq(left, right, qty),
        _ => unimplemented!(),
    };

    if let Some((new_left, new_right)) = res {
        let new_exp = Exp::RelationChain {
            rels,
            terms: vec![new_left, new_right],
        };
        Ok(Some(new_exp))
    } else {
        Ok(None)
    }
}

fn isolate_eq(left: Exp, right: Exp, qty: Exp) -> Option<(Exp, Exp)> {
    None
}
