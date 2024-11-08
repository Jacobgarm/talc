use std::rc::Rc;

use crate::linalg;

pub mod operators;
pub use operators::*;

pub mod procedures;
pub use procedures::*;

pub mod numeric;
pub use numeric::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Exp {
    Number(Numeric),
    Bool(bool),
    ImagUnit,
    Inf,
    Var {
        name: String,
    },
    Unary {
        op: operators::UnaryOp,
        operand: Rc<Exp>,
    },
    Dyadic {
        op: operators::DyadicOp,
        left: Rc<Exp>,
        right: Rc<Exp>,
    },
    Pool {
        op: operators::AssocOp,
        terms: Vec<Exp>,
    },
    RelationChain {
        rels: Vec<operators::Relation>,
        terms: Vec<Exp>,
    },
    Function {
        name: String,
        primes: Vec<usize>,
        args: Vec<Exp>,
    },
    Procedure {
        kind: ProcedureKind,
        args: Vec<Vec<Exp>>,
    },
    Matrix(linalg::Matrix),
}

impl Exp {
    pub const ZERO: Self = Self::Number(Numeric::ZERO_INT);
    pub const ONE: Self = Self::Number(Numeric::ONE_INT);
    pub const NEGATIVE_ONE: Self = Self::Number(Numeric::NEGATIVE_ONE_INT);

    pub fn is_atomic(&self) -> bool {
        use Exp::*;
        match self {
            Number(..) | Bool(..) | ImagUnit | Inf | Var { .. } => true,
            Unary { .. }
            | Dyadic { .. }
            | Pool { .. }
            | RelationChain { .. }
            | Function { .. }
            | Procedure { .. }
            | Matrix { .. } => false,
        }
    }

    pub fn assoc_combine(op: AssocOp, first: Self, second: Self) -> Self {
        let mut terms = Vec::new();

        match first {
            Self::Pool {
                op: first_op,
                terms: mut first_terms,
            } if first_op == op => terms.append(&mut first_terms),
            _ => terms.push(first),
        }

        match second {
            Self::Pool {
                op: second_op,
                terms: mut second_terms,
            } if second_op == op => terms.append(&mut second_terms),
            _ => terms.push(second),
        }

        Self::Pool { op, terms }
    }

    pub fn chain_combine(
        rel: Relation,
        first: Self,
        second: Self,
        open_first: bool,
        open_second: bool,
    ) -> Self {
        let mut terms = Vec::new();
        let mut rels = Vec::new();
        match first {
            Self::RelationChain {
                rels: mut first_rels,
                terms: mut first_terms,
            } if open_first => {
                terms.append(&mut first_terms);
                rels.append(&mut first_rels);
            }
            _ => terms.push(first),
        }

        rels.push(rel);
        match second {
            Self::RelationChain {
                rels: mut second_rels,
                terms: mut second_terms,
            } if open_second => {
                terms.append(&mut second_terms);
                rels.append(&mut second_rels);
            }
            _ => terms.push(second),
        }

        Self::RelationChain { rels, terms }
    }
}

impl From<Numeric> for Exp {
    fn from(value: Numeric) -> Self {
        Self::Number(value)
    }
}
