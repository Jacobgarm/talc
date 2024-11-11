use ordered_hash_map::OrderedHashMap;

use crate::ast::{AssocOp, Exp, Numeric};
use crate::context::Context;
use crate::eval::{EvalError, EvalResult};
use crate::linalg::Matrix;

pub fn flatten_pool(op: AssocOp, terms: Vec<Exp>) -> Vec<Exp> {
    terms
        .into_iter()
        .flat_map(|term| match term {
            Exp::Pool {
                op: sub_op,
                terms: sub_terms,
            } if sub_op == op => sub_terms,
            _ => vec![term],
        })
        .collect()
}

pub fn add_fold(terms: Vec<Exp>, ctx: &Context) -> EvalResult<Vec<Exp>> {
    if terms.len() <= 1 {
        return Ok(terms.to_vec());
    }

    let mut total_num = Numeric::ZERO_INT;
    let mut total_mat: Option<Matrix> = None;
    let mut leftovers = OrderedHashMap::new();

    let flattened_terms = flatten_pool(AssocOp::Add, terms);

    for term in flattened_terms.into_iter() {
        match term {
            Exp::Number(num) => {
                if total_mat.is_some() {
                    return Err(EvalError::MatrixScalarSum);
                }
                total_num = &total_num + &num;
            }
            Exp::Matrix(mat) => {
                total_mat = Some(match total_mat {
                    None => mat,
                    Some(cur_mat) => {
                        if cur_mat.size() != mat.size() {
                            return Err(EvalError::MatrixSizesIncompatibleAdd {
                                left_size: cur_mat.size(),
                                right_size: mat.size(),
                            });
                        } else {
                            cur_mat + mat
                        }
                    }
                })
            }
            Exp::Pool {
                op: AssocOp::Mul,
                terms: mut mul_terms,
            } if mul_terms
                .first()
                .is_some_and(|term| matches!(term, Exp::Number(..))) =>
            {
                let Exp::Number(inner_coef) = mul_terms.remove(0) else {
                    unreachable!()
                };
                let rest = if mul_terms.len() == 1 {
                    mul_terms.pop().unwrap()
                } else {
                    Exp::Pool {
                        op: AssocOp::Mul,
                        terms: mul_terms,
                    }
                };
                if let Some(coef) = leftovers.get_mut(&rest) {
                    *coef = &inner_coef + coef;
                } else {
                    leftovers.insert(rest, inner_coef);
                };
            }
            exp => {
                if let Some(coef) = leftovers.get_mut(&exp) {
                    *coef = &Numeric::ONE_INT + coef;
                } else {
                    leftovers.insert(exp, Numeric::ONE_INT);
                };
            }
        }
    }

    let mut new_terms = Vec::new();

    if let Some(mat) = total_mat {
        new_terms.push(mat.try_map(|exp| exp.eval(ctx))?.into());
    } else if !total_num.is_zero() {
        new_terms.push(total_num.into());
    }

    for (term, coef) in leftovers.into_iter().filter(|(_, c)| !c.is_zero()) {
        let full_term = if coef.is_one() {
            term
        } else {
            Exp::assoc_combine(AssocOp::Mul, coef.into(), term)
        };
        new_terms.push(full_term);
    }

    Ok(new_terms)
}
