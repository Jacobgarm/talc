use std::collections::HashSet;

use itertools::Itertools;

use crate::{context::Context, linalg};

pub mod operators;
pub use operators::*;

pub mod procedures;
pub use procedures::*;

pub mod numeric;
pub use numeric::*;

pub mod complex;
pub use complex::*;

pub mod substitute;

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
        op: UnaryOp,
        operand: Box<Exp>,
    },
    Dyadic {
        op: DyadicOp,
        left: Box<Exp>,
        right: Box<Exp>,
    },
    Pool {
        op: AssocOp,
        terms: Vec<Exp>,
    },
    RelationChain {
        rels: Vec<Relation>,
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
    Tuple(Vec<Exp>),
    Matrix(linalg::Matrix),
}

impl Exp {
    pub const ZERO: Self = Self::Real(RealNum::ZERO_INT);
    pub const ONE: Self = Self::Real(RealNum::ONE_INT);
    pub const NEGATIVE_ONE: Self = Self::Real(RealNum::NEGATIVE_ONE_INT);

    /// No fields are Exps
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
            | Tuple(_)
            | Matrix { .. } => false,
        }
    }

    /// Value is independent of context and atomic quantity
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
            | Tuple(_)
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
            Tuple(entries) => Tuple(entries.iter().map(|exp| f(exp)).collect()),
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
            Tuple(entries) => Tuple(entries.iter().map(|exp| f(exp)).try_collect()?),
            Matrix(mat) => Matrix(mat.try_map(f)?),
        })
    }

    pub fn reduce<T, F, J, D>(&self, f: F, joiner: J, def: D) -> T
    where
        F: Fn(&Self) -> T,
        J: Fn(T, T) -> T,
        D: Fn() -> T,
    {
        use Exp::*;
        match self {
            Real(..) | Complex(..) | Inf | Var { .. } | Bool(..) => def(),
            Unary { operand, .. } => f(operand),
            Dyadic { left, right, .. } => joiner(f(left), f(right)),
            Pool { terms, .. } | RelationChain { terms, .. } => {
                terms.iter().map(f).reduce(joiner).unwrap_or_else(def)
            }
            Function { args, .. } => args.iter().map(f).reduce(joiner).unwrap_or_else(def),
            Procedure { args, .. } => args
                .iter()
                .flatten()
                .map(f)
                .reduce(joiner)
                .unwrap_or_else(def),
            Tuple(entries) => entries.iter().map(f).reduce(joiner).unwrap_or_else(def),
            Matrix(mat) => mat.iter_by_rows().map(f).reduce(joiner).unwrap_or_else(def),
        }
    }

    pub fn contains(&self, sub_exp: &Exp) -> bool {
        if self == sub_exp {
            true
        } else {
            self.reduce(|exp| exp.contains(sub_exp), |a, b| a || b, || false)
        }
    }

    pub fn all_vars(&self) -> HashSet<String> {
        if let Self::Var { name } = self {
            HashSet::from([name.clone()])
        } else {
            self.reduce(Self::all_vars, |a, b| &a | &b, HashSet::new)
        }
    }

    pub fn vars(&self) -> HashSet<String> {
        use ProcedureKind as PK;
        let vs = |exp: &Exp| exp.reduce(Self::vars, |a, b| &a | &b, HashSet::new);
        match self {
            Self::Var { name } => HashSet::from([name.clone()]),
            Self::Procedure {
                kind: PK::Isolate,
                args,
            } => vs(&args[0][0]),
            exp => vs(exp),
        }
    }

    pub fn functions(&self) -> HashSet<String> {
        let mut funcs = self.reduce(Self::functions, |a, b| &a | &b, HashSet::new);
        if let Self::Function { name, .. } = self {
            funcs.insert(name.clone());
        }
        funcs
    }

    pub fn depends_on_var(&self, var: &str, ctx: &Context) -> bool {
        match self {
            Self::Var { name } => {
                return if name == var {
                    true
                } else if let Some(var_info) = ctx.get_var(name) {
                    var_info.exp.depends_on_var(var, ctx)
                } else {
                    false
                };
            }
            Self::Function { name, .. } => {
                if let Some(func_info) = ctx.get_func(name) {
                    if let Some(exp) = &func_info.exp {
                        if exp.depends_on_var(var, ctx) {
                            return true;
                        }
                    }
                }
            }
            _ => (),
        }
        self.reduce(|exp| exp.depends_on_var(var, ctx), |a, b| a || b, || false)
    }

    pub fn depends_on(&self, exp: &Exp, ctx: &Context) -> bool {
        if self == exp {
            return true;
        }
        match self {
            Self::Var { name } => {
                return if let Some(var_info) = ctx.get_var(name) {
                    var_info.exp.depends_on(exp, ctx)
                } else {
                    false
                };
            }
            Self::Function { name, .. } => {
                if let Some(func_info) = ctx.get_func(name) {
                    if let Some(exp) = &func_info.exp {
                        if exp.depends_on(exp, ctx) {
                            return true;
                        }
                    }
                }
            }
            _ => (),
        }
        self.reduce(|e| e.depends_on(exp, ctx), |a, b| a || b, || false)
    }

    pub fn pow(self, exponent: Self) -> Self {
        Self::Dyadic {
            op: DyadicOp::Pow,
            left: self.into(),
            right: exponent.into(),
        }
    }
}

impl std::ops::Add for Exp {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::assoc_combine(AssocOp::Add, self, rhs)
    }
}

impl std::ops::Neg for Exp {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self::assoc_combine(AssocOp::Mul, Self::NEGATIVE_ONE, self)
    }
}

impl std::ops::Sub for Exp {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl std::ops::Mul for Exp {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self::assoc_combine(AssocOp::Mul, self, rhs)
    }
}

impl std::ops::Div for Exp {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Self::assoc_combine(AssocOp::Mul, self, rhs.pow(Self::NEGATIVE_ONE))
    }
}
