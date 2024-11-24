use itertools::Itertools;

use crate::linalg;

pub mod operators;
pub use operators::*;

pub mod procedures;
pub use procedures::*;

pub mod numeric;
pub use numeric::*;

pub mod complex;
pub use complex::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Exp {
    Real(RealNum),
    Complex(ComplexNum),
    Bool(bool),
    Inf,
    Var {
        name: String,
    },
    Unary {
        op: operators::UnaryOp,
        operand: Box<Exp>,
    },
    Dyadic {
        op: operators::DyadicOp,
        left: Box<Exp>,
        right: Box<Exp>,
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
    pub const ZERO: Self = Self::Real(RealNum::ZERO_INT);
    pub const ONE: Self = Self::Real(RealNum::ONE_INT);
    pub const NEGATIVE_ONE: Self = Self::Real(RealNum::NEGATIVE_ONE_INT);

    // No fields are Exps
    pub fn is_atomic(&self) -> bool {
        use Exp::*;
        match self {
            Real(..) | Complex(..) | Bool(..) | Inf | Var { .. } => true,
            Unary { .. }
            | Dyadic { .. }
            | Pool { .. }
            | RelationChain { .. }
            | Function { .. }
            | Procedure { .. }
            | Matrix { .. } => false,
        }
    }

    // Valued is a fixed, simple quantity
    pub fn is_simple(&self) -> bool {
        use Exp::*;
        match self {
            Real(..) | Complex(..) | Bool(..) | Inf => true,
            Var { .. }
            | Unary { .. }
            | Dyadic { .. }
            | Pool { .. }
            | RelationChain { .. }
            | Function { .. }
            | Procedure { .. }
            | Matrix { .. } => false,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Self::Real(..) | Self::Complex(..))
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

    pub fn map<F>(&self, mut f: F) -> Exp
    where
        F: FnMut(&Exp) -> Exp,
    {
        use Exp::*;
        match self {
            Real(..) | Complex(..) | Inf | Var { .. } | Bool(..) => self.clone(),
            Unary { op, operand } => Unary {
                op: *op,
                operand: f(operand).into(),
            },
            Dyadic { op, left, right } => Dyadic {
                op: *op,
                left: f(left).into(),
                right: f(right).into(),
            },
            Pool { op, terms } => Pool {
                op: *op,
                terms: terms.iter().map(f).collect_vec(),
            },
            RelationChain { rels, terms } => RelationChain {
                rels: rels.clone(),
                terms: terms.iter().map(f).collect_vec(),
            },
            Function { name, primes, args } => Function {
                name: name.clone(),
                primes: primes.clone(),
                args: args.iter().map(f).collect_vec(),
            },
            Procedure { kind, args } => Procedure {
                kind: *kind,
                args: args
                    .iter()
                    .map(|row| row.iter().map(&mut f).collect_vec())
                    .collect_vec(),
            },
            Matrix(mat) => Matrix(mat.map(f)),
        }
    }

    pub fn try_map<F, E>(&self, mut f: F) -> Result<Exp, E>
    where
        F: FnMut(&Exp) -> Result<Exp, E>,
    {
        use Exp::*;
        Ok(match self {
            Real(..) | Complex(..) | Inf | Var { .. } | Bool(..) => self.clone(),
            Unary { op, operand } => Unary {
                op: *op,
                operand: f(operand)?.into(),
            },
            Dyadic { op, left, right } => Dyadic {
                op: *op,
                left: f(left)?.into(),
                right: f(right)?.into(),
            },
            Pool { op, terms } => Pool {
                op: *op,
                terms: terms.iter().map(f).try_collect()?,
            },
            RelationChain { rels, terms } => RelationChain {
                rels: rels.clone(),
                terms: terms.iter().map(f).try_collect()?,
            },
            Function { name, primes, args } => Function {
                name: name.clone(),
                primes: primes.clone(),
                args: args.iter().map(f).try_collect()?,
            },
            Procedure { kind, args } => Procedure {
                kind: *kind,
                args: args
                    .iter()
                    .map(|row| row.iter().map(&mut f).try_collect())
                    .try_collect()?,
            },
            Matrix(mat) => Matrix(mat.try_map(f)?),
        })
    }
}
