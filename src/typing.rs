use crate::ast::{AssocOp, DyadicOp, Exp, ProcedureKind, UnaryOp};
use crate::context::{Context, DEFAULT_CONTEXT};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpType {
    Unknown,
    Numeric,
    Matrix,
    Bool,
    Set,
}

impl Exp {
    pub fn infer_type(&self, ctx: &Context) -> ExpType {
        match self {
            Exp::Number(..) | Exp::ImagUnit | Exp::Inf => ExpType::Numeric,
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
                if let Some(info) = ctx.get_function(name) {
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
            Exp::Matrix(..) => ExpType::Matrix,
        }
    }
}

fn procedure_type(kind: ProcedureKind, args: &[Vec<Exp>], ctx: &Context) -> ExpType {
    use ProcedureKind::*;
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
