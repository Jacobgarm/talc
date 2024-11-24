use std::ops::AddAssign;

use ordermap::{OrderMap, OrderSet};

use crate::ast::{AssocOp, ComplexNum, DyadicOp, Exp, RealNum, UnaryOp};
use crate::context::Context;
use crate::eval::{EvalError, EvalResult};
use crate::linalg::{self, Matrix};

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
enum AddHead {
    Empty,
    Real(RealNum),
    Complex(ComplexNum),
    Mat(Matrix),
}

#[derive(Debug, Clone)]
enum Number {
    Real(RealNum),
    Complex(ComplexNum),
}

impl Number {
    fn simplify(self) -> Self {
        if let Self::Complex(num) = self {
            if num.is_real() {
                Self::Real(num.real)
            } else {
                Self::Complex(num)
            }
        } else {
            self
        }
    }
}

impl AddAssign<&Number> for Number {
    fn add_assign(&mut self, rhs: &Number) {
        use Number::*;
        *self = match (&self, rhs) {
            (Real(a), Real(b)) => Real(a + &b),
            (Complex(a), Real(b)) => Complex(a + &b.clone().into()),
            (Real(a), Complex(b)) => Complex(&ComplexNum::from(a.clone()) + b),
            (Complex(a), Complex(b)) => Complex(a + &b),
        }
    }
}

pub fn add_fold(terms: Vec<Exp>, ctx: &Context) -> EvalResult<Vec<Exp>> {
    use AddHead::*;

    if terms.len() <= 1 {
        return Ok(terms.to_vec());
    }

    let mut cur_head = AddHead::Empty;
    let mut leftovers = OrderMap::new();

    for term in terms.into_iter() {
        match term {
            Exp::Real(num) => {
                cur_head = match cur_head {
                    Empty => Real(num),
                    Real(head) => Real(head + num),
                    Complex(head) => Complex(head + num.into()),
                    Mat(_) => return Err(EvalError::MatrixScalarSum),
                };
            }
            Exp::Complex(num) => {
                cur_head = match cur_head {
                    Empty => Complex(num),
                    Real(head) => Complex(ComplexNum::from(head) + num),
                    Complex(head) => Complex(head + num),
                    Mat(_) => return Err(EvalError::MatrixScalarSum),
                }
            }
            Exp::Matrix(mat) => {
                cur_head = match cur_head {
                    Empty => Mat(mat),
                    Mat(head) => {
                        if head.size() != mat.size() {
                            return Err(EvalError::MatrixSizesIncompatibleAdd {
                                left_size: head.size(),
                                right_size: mat.size(),
                            });
                        } else {
                            Mat(head + mat)
                        }
                    }
                    Real(..) | Complex(..) => return Err(EvalError::MatrixScalarSum),
                };
            }
            Exp::Pool {
                op: AssocOp::Mul,
                terms: mut mul_terms,
            } if mul_terms.first().is_some_and(|term| term.is_number()) => {
                let inner_coef = match mul_terms.remove(0) {
                    Exp::Real(num) => Number::Real(num),
                    Exp::Complex(num) => Number::Complex(num),
                    _ => unreachable!(),
                };
                let rest = if mul_terms.len() == 1 {
                    mul_terms.pop().unwrap()
                } else {
                    Exp::Pool {
                        op: AssocOp::Mul,
                        terms: mul_terms,
                    }
                };
                leftovers
                    .entry(rest)
                    .and_modify(|coef| *coef += &inner_coef)
                    .or_insert(inner_coef);
            }
            exp => {
                leftovers
                    .entry(exp)
                    .and_modify(|coef| *coef += &Number::Real(RealNum::ONE_INT))
                    .or_insert(Number::Real(RealNum::ONE_INT));
            }
        }
    }

    let mut new_terms = Vec::new();

    match cur_head {
        Real(num) if !num.is_zero() => new_terms.push(num.into()),
        Complex(num) if !num.is_zero() => new_terms.push(num.into()),
        Mat(mat) => new_terms.push(mat.try_map(|exp| exp.eval(ctx))?.into()),
        _ => (),
    }

    for (term, mut coef) in leftovers.into_iter() {
        coef = coef.simplify();
        let full_term = match coef {
            Number::Real(num) => {
                if num.is_zero() {
                    continue;
                } else if num.is_one() {
                    term
                } else {
                    Exp::assoc_combine(AssocOp::Mul, num.into(), term)
                }
            }
            Number::Complex(num) => Exp::assoc_combine(AssocOp::Mul, num.into(), term),
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
    let mut leftovers = OrderMap::new();

    for term in terms.into_iter() {
        dbg!(&term);
        match term {
            Exp::Real(num) => {
                cur_num_head = Some(match cur_num_head {
                    None => Number::Real(num),
                    Some(Number::Real(cur)) => Number::Real(cur * num),
                    Some(Number::Complex(cur)) => Number::Complex(cur * num.into()),
                })
            }
            Exp::Complex(num) => {
                cur_num_head = Some(match cur_num_head {
                    None => Number::Complex(num),
                    Some(Number::Real(cur)) => Number::Complex(ComplexNum::from(cur) * num),
                    Some(Number::Complex(cur)) => Number::Complex(cur * num),
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
                right: box pow,
            } if pow.is_number() => {
                let new_pow = match pow {
                    Exp::Real(num) => Number::Real(num),
                    Exp::Complex(num) => Number::Complex(num),
                    _ => unreachable!(),
                };
                leftovers
                    .entry(*left)
                    .and_modify(|pow| *pow += &new_pow)
                    .or_insert(new_pow);
            }

            exp => {
                leftovers
                    .entry(exp)
                    .and_modify(|pow| *pow += &Number::Real(RealNum::ONE_INT))
                    .or_insert(Number::Real(RealNum::ONE_INT));
            }
        }
    }

    let mut new_terms = Vec::new();

    cur_num_head = cur_num_head.map(Number::simplify);

    match cur_num_head {
        Some(Number::Real(num)) if !num.is_exact_one() => {
            if num.is_zero() {
                if non_commuting_terms.len() == 1
                    && let Exp::Matrix(mat) = non_commuting_terms.pop().unwrap()
                {
                    new_terms.push(linalg::Matrix::filled(num.into(), mat.size()).into());
                } else {
                    new_terms.push(num.into());
                    new_terms.append(&mut non_commuting_terms);
                }
                return Ok(new_terms);
            } else {
                new_terms.push(num.into());
            }
        }
        Some(Number::Complex(num)) => new_terms.push(num.into()),
        _ => (),
    }

    for (term, mut pow) in leftovers.into_iter() {
        pow = pow.simplify();
        let full_term = match pow {
            Number::Real(num) => {
                if num.is_exact_zero() {
                    continue;
                } else if num.is_exact_one() {
                    term
                } else {
                    Exp::Dyadic {
                        op: DyadicOp::Pow,
                        left: term.into(),
                        right: Exp::Real(num).into(),
                    }
                }
            }
            Number::Complex(num) => Exp::Dyadic {
                op: DyadicOp::Pow,
                left: term.into(),
                right: Exp::Complex(num).into(),
            },
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

    if new_terms.is_empty() {
        new_terms.push(Exp::ONE);
    }

    Ok(new_terms)
}

pub fn and_or_fold(is_or: bool, terms: Vec<Exp>, _ctx: &Context) -> EvalResult<Vec<Exp>> {
    let mut new_terms: OrderSet<Exp> = OrderSet::new();

    for term in terms.into_iter() {
        if let Exp::Bool(b) = term {
            if b == is_or {
                return Ok(vec![Exp::Bool(is_or)]);
            } else {
                continue;
            }
        }

        if let Exp::Unary {
            op: UnaryOp::Not,
            operand: box ref oper,
        } = term
        {
            if new_terms.contains(oper) {
                return Ok(vec![Exp::Bool(is_or)]);
            }
        } else if new_terms.contains(&Exp::Unary {
            op: UnaryOp::Not,
            operand: term.clone().into(),
        }) {
            return Ok(vec![Exp::Bool(is_or)]);
        }

        new_terms.insert(term);
    }

    if new_terms.is_empty() {
        return Ok(vec![Exp::Bool(!is_or)]);
    }

    Ok(new_terms.into_iter().collect())
}

pub fn xor_fold(terms: Vec<Exp>, _ctx: &Context) -> EvalResult<Vec<Exp>> {
    let mut new_terms = OrderSet::new();

    let mut odd_trues = false;

    for term in terms.into_iter() {
        match term {
            Exp::Bool(true) => odd_trues = !odd_trues,
            Exp::Bool(false) => continue,
            exp => {
                if new_terms.contains(&exp) {
                    new_terms.remove(&exp);
                } else {
                    new_terms.insert(exp);
                }
            }
        }
    }

    let mut new_new_terms = new_terms.clone();

    for term in &new_terms {
        if let Exp::Unary {
            op: UnaryOp::Not,
            operand: box operand,
        } = term
        {
            if new_new_terms.contains(operand) {
                new_new_terms.remove(term);
                new_new_terms.remove(operand);
                odd_trues = !odd_trues;
            }
        }
    }

    if odd_trues {
        if new_new_terms.len() == 1 {
            return Ok(vec![Exp::Unary {
                op: UnaryOp::Not,
                operand: new_new_terms.pop().unwrap().into(),
            }]);
        } else {
            new_new_terms.shift_insert(0, Exp::Bool(true));
        }
    }

    if new_new_terms.is_empty() {
        new_new_terms.insert(Exp::Bool(false));
    }

    Ok(new_new_terms.into_iter().collect())
}
