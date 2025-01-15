use std::fmt::Display;

use crate::ast::{AssocOp, DyadicOp, Exp, ProcedureKind, UnaryOp};
use crate::context::{Context, DEFAULT_CONTEXT};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpType {
    Unknown,
    Numeric,
    Matrix,
    Tuple,
    Bool,
    Set,
}

impl ExpType {
    pub fn is_unknown_or(self, ty: Self) -> bool {
        self == Self::Unknown || self == ty
    }
}

impl Display for ExpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExpType::*;
        let s = match self {
            Unknown => "UnknownType",
            Numeric => "Numeric",
            Matrix => "Matrix",
            Tuple => "Tuple",
            Bool => "Bool",
            Set => "Set",
        };
        write!(f, "{s}")
    }
}

impl Exp {
    pub fn infer_type(&self, ctx: &Context) -> ExpType {
        match self {
            Exp::Real(..) | Exp::Complex(..) | Exp::Inf => ExpType::Numeric,
            Exp::Bool(..) => ExpType::Bool,
            Exp::Var { name } => {
                if let Some(info) = ctx.get_var(name) {
                    info.exp.infer_type(ctx)
                } else {
                    ExpType::Unknown
                }
            }
            Exp::Unary { op, .. } => match op {
                UnaryOp::Factorial
                | UnaryOp::Abs
                | UnaryOp::Arg
                | UnaryOp::Floor
                | UnaryOp::Ceil
                | UnaryOp::Norm => ExpType::Numeric,
                UnaryOp::Not => ExpType::Bool,
            },
            Exp::Dyadic { op, left, .. } => match op {
                DyadicOp::Mod | DyadicOp::DotProd => ExpType::Numeric,
                DyadicOp::Pow => left.infer_type(ctx),
                DyadicOp::Index => entry_type(left, None, ctx),
                DyadicOp::CrossProd => ExpType::Matrix,
                DyadicOp::LogicEquiv | DyadicOp::LogicImplies => ExpType::Bool,
                DyadicOp::SetDifference | DyadicOp::SymDifference => ExpType::Set,
            },
            Exp::Pool { op, terms } => match op {
                AssocOp::Union | AssocOp::Intersection => ExpType::Set,
                AssocOp::LogicOr | AssocOp::LogicAnd | AssocOp::LogicXor => ExpType::Bool,
                AssocOp::Add => {
                    let mut exp_type = ExpType::Unknown;
                    for term in terms {
                        let term_type = term.infer_type(ctx);
                        if matches!(term_type, ExpType::Numeric | ExpType::Matrix) {
                            exp_type = term_type;
                            break;
                        }
                    }
                    exp_type
                }
                AssocOp::Mul => {
                    let mut exp_type = ExpType::Unknown;
                    for term in terms {
                        let term_type = term.infer_type(ctx);
                        match term_type {
                            ExpType::Numeric => exp_type = term_type,
                            ExpType::Matrix => {
                                exp_type = term_type;
                                break;
                            }
                            _ => (),
                        }
                    }
                    exp_type
                }
            },
            Exp::RelationChain { .. } => ExpType::Bool,
            Exp::Function { name, .. } => {
                if let Some(info) = ctx.get_func(name) {
                    if info.float_func.is_some() {
                        ExpType::Numeric
                    } else if let Some(exp) = &info.exp {
                        exp.infer_type(ctx)
                    } else {
                        ExpType::Unknown
                    }
                } else {
                    ExpType::Unknown
                }
            }
            Exp::Procedure { kind, args } => procedure_type(*kind, args, ctx),
            Exp::Tuple(_) => ExpType::Tuple,
            Exp::Matrix(..) => ExpType::Matrix,
        }
    }
}

fn entry_type(exp: &Exp, _index: Option<&[usize]>, ctx: &Context) -> ExpType {
    // TODO add more cases
    match exp {
        Exp::Matrix(mat) => {
            if mat.size() == (0, 0) {
                ExpType::Unknown
            } else {
                mat[(0, 0)].infer_type(ctx)
            }
        }
        Exp::Tuple(entries) => {
            if entries.is_empty() {
                ExpType::Unknown
            } else {
                entries[0].infer_type(ctx)
            }
        }
        _ => ExpType::Unknown,
    }
}

fn procedure_type(kind: ProcedureKind, args: &[Vec<Exp>], ctx: &Context) -> ExpType {
    use ProcedureKind::*;
    #[allow(clippy::match_same_arms)]
    match kind {
        Approximate => args[0][0].infer_type(ctx),
        Assuming => args[0][0].infer_type(ctx),
        Piecewise => args[0][0].infer_type(ctx),
        Derivative | Integral | Limit => args[0][0].infer_type(ctx),
        Sum | Product => args[0][0].infer_type(ctx),

        Simplify | Expand | Collect => args[0][0].infer_type(ctx),
        ColumnVector | RowVector | Matrix | DiagonalMatrix | IdentityMatrix => ExpType::Matrix,
        Gradient | Hessian | Jacobian => ExpType::Matrix,
        EvalDefaultContext => DEFAULT_CONTEXT.with_borrow(|def_ctx| args[0][0].infer_type(def_ctx)),
        _ => ExpType::Unknown,
    }
}
