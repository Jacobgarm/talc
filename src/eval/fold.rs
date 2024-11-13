use std::borrow::Borrow;

use malachite::Integer;
use ordered_hash_map::OrderedHashMap;

use crate::ast::{AssocOp, DyadicOp, Exp, Numeric};
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

#[derive(Debug, Clone)]
enum NumOrMat {
    Neither,
    Num(Numeric),
    Mat(Matrix),
}

pub fn add_fold(terms: Vec<Exp>, ctx: &Context) -> EvalResult<Vec<Exp>> {
    if terms.len() <= 1 {
        return Ok(terms.to_vec());
    }

    let mut cur_head = NumOrMat::Neither;
    let mut leftovers = OrderedHashMap::new();

    let flattened_terms = flatten_pool(AssocOp::Add, terms);

    for term in flattened_terms.into_iter() {
        match term {
            Exp::Number(num) => {
                cur_head = match cur_head {
                    NumOrMat::Neither => NumOrMat::Num(num),
                    NumOrMat::Num(head) => NumOrMat::Num(head + num),
                    NumOrMat::Mat(_) => return Err(EvalError::MatrixScalarSum),
                };
            }
            Exp::Matrix(mat) => {
                cur_head = match cur_head {
                    NumOrMat::Neither => NumOrMat::Mat(mat),
                    NumOrMat::Mat(head) => {
                        if head.size() != mat.size() {
                            return Err(EvalError::MatrixSizesIncompatibleAdd {
                                left_size: head.size(),
                                right_size: mat.size(),
                            });
                        } else {
                            NumOrMat::Mat(head + mat)
                        }
                    }
                    NumOrMat::Num(_) => return Err(EvalError::MatrixScalarSum),
                };
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

    match cur_head {
        NumOrMat::Num(num) if !num.is_one() => new_terms.push(num.into()),
        NumOrMat::Mat(mat) => new_terms.push(mat.try_map(|exp| exp.eval(ctx))?.into()),
        _ => (),
    }

    for (term, coef) in leftovers.into_iter().filter(|(_, c)| !c.is_zero()) {
        let full_term = if coef.is_one() {
            term
        } else {
            Exp::assoc_combine(AssocOp::Mul, coef.into(), term)
        };
        new_terms.push(full_term);
    }

    if new_terms.is_empty() {
        new_terms.push(Exp::ZERO);
    }

    Ok(new_terms)
}

pub fn mul_fold(terms: Vec<Exp>, ctx: &Context) -> EvalResult<Vec<Exp>> {
    if terms.len() <= 1 {
        return Ok(terms);
    }

    let mut cur_num_head = None;
    let mut non_commuting_terms = Vec::new();
    let mut leftovers = OrderedHashMap::new();

    let flattened_terms = flatten_pool(AssocOp::Mul, terms);

    for term in flattened_terms.into_iter() {
        match term {
            Exp::Number(num) => {
                cur_num_head = Some(match cur_num_head {
                    None => num,
                    Some(cur) => cur * num,
                })
            }
            Exp::Matrix(mat) => {
                if let Some(tail) = non_commuting_terms.pop() {
                    if let Exp::Matrix(tail_mat) = tail {
                        non_commuting_terms.push((tail_mat * mat).into());
                    } else {
                        non_commuting_terms.push(tail);
                        non_commuting_terms.push(mat.into())
                    }
                } else {
                    non_commuting_terms.push(mat.into());
                }
            }
            exp if exp.infer_type(ctx) == crate::typing::ExpType::Matrix => {
                non_commuting_terms.push(exp);
            }
            Exp::Dyadic {
                op: DyadicOp::Pow,
                left,
                right: box Exp::Number(num),
            } => {
                if let Some(pow) = leftovers.get_mut(&*left) {
                    *pow = &*pow + &num;
                } else {
                    leftovers.insert(*left, num);
                };
            }

            exp => {
                if let Some(pow) = leftovers.get_mut(&exp) {
                    *pow = &*pow + &Numeric::ONE_INT;
                } else {
                    leftovers.insert(exp, Numeric::ONE_INT);
                };
            }
        }
    }

    let mut new_terms = Vec::new();

    if let Some(num) = cur_num_head
        && !num.is_one()
    {
        new_terms.push(num.into());
    }

    for (term, pow) in leftovers.into_iter().filter(|(_, pow)| !pow.is_zero()) {
        let full_term = if pow.is_one() {
            term
        } else {
            Exp::Dyadic {
                op: DyadicOp::Pow,
                left: term.into(),
                right: Exp::Number(pow).into(),
            }
        };
        new_terms.push(full_term);
    }

    if !non_commuting_terms.is_empty() {
        if non_commuting_terms.len() == 1
            && let Exp::Matrix(mat) = non_commuting_terms.pop().unwrap()
        {
            let coef = Exp::Pool {
                op: AssocOp::Mul,
                terms: new_terms,
            };
            let new_mat = mat.scale(&coef);
            new_terms = vec![new_mat.try_map(|exp| exp.eval(ctx))?.into()];
        } else {
            new_terms.append(&mut non_commuting_terms);
        }
    }

    Ok(new_terms)
}
