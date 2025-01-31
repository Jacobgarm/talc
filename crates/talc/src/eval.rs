use crate::ast::{AssocOp, ComplexNum, DyadicOp, Exp, RealNum, UnaryOp};
use crate::context::{ApproxLevel, Context};
use crate::typing::ExpType;

mod dyadic;
mod error;
mod fold;
mod functions;
mod procedures;
mod relations;
mod unary;

pub use error::*;

impl Exp {
    pub fn eval(&self, ctx: &Context) -> EvalResult<Exp> {
        use Exp::*;

        let exp = if let Procedure { kind, .. } = self
            && !kind.eagerly_eval_args()
        {
            self.clone()
        } else {
            self.try_map(|exp| exp.eval(ctx))?
        };

        Ok(match exp {
            Real(num) => eval_real(num, ctx).into(),
            Complex(num) => eval_complex(num, ctx),
            Var { ref name } => {
                if let Some(info) = ctx.get_var(name) {
                    let var_exp = &info.exp;
                    if let Real(RealNum::Small(_)) = var_exp
                        && ctx.approx_level != ApproxLevel::SmallFloat
                    {
                        exp
                    } else {
                        info.exp.clone()
                    }
                } else {
                    exp
                }
            }

            Unary { op, operand } => eval_unary(op, *operand, ctx)?,
            Dyadic { op, left, right } => eval_dyadic(op, *left, *right, ctx)?,
            Pool { op, terms } => eval_pool(op, terms, ctx)?,
            RelationChain { rels, terms } => relations::eval_chain(rels, terms, ctx)?,
            Procedure { kind, args } => procedures::eval_procedure(kind, args, ctx)?,
            Function { name, primes, args } => functions::eval_function(name, primes, args, ctx)?,
            _ => exp,
        })
    }

    pub fn approximate(&self, prec: Option<u64>, ctx: &Context) -> EvalResult<Self> {
        let mut new_context = ctx.clone();
        new_context.approx_level = if let Some(val) = prec {
            ApproxLevel::BigFloat(val)
        } else {
            ApproxLevel::SmallFloat
        };
        self.eval(&new_context)
    }
}

fn eval_real(num: RealNum, ctx: &Context) -> RealNum {
    match ctx.approx_level {
        ApproxLevel::None => num,
        ApproxLevel::SmallFloat => num.smallify(),
        ApproxLevel::BigFloat(prec) => num.bigify(prec),
    }
}

fn eval_complex(num: ComplexNum, ctx: &Context) -> Exp {
    if num.imag.is_zero() {
        eval_real(num.real, ctx).into()
    } else {
        ComplexNum {
            real: eval_real(num.real, ctx),
            imag: eval_real(num.imag, ctx),
        }
        .into()
    }
}

fn eval_unary(op: UnaryOp, exp: Exp, ctx: &Context) -> EvalResult<Exp> {
    match op {
        UnaryOp::Not => unary::eval_not(exp, ctx),
        UnaryOp::Abs => unary::eval_abs(exp, ctx),
        UnaryOp::Norm => unary::eval_norm(exp, ctx),
        UnaryOp::Factorial => unary::eval_factorial(exp, ctx),
        _ => Ok(Exp::Unary {
            op,
            operand: exp.into(),
        }),
    }
}

fn eval_dyadic(op: DyadicOp, left: Exp, right: Exp, ctx: &Context) -> EvalResult<Exp> {
    match op {
        DyadicOp::LogicEquiv => dyadic::eval_equiv(left, right, ctx),
        DyadicOp::LogicImplies => dyadic::eval_implies(left, right, ctx),
        DyadicOp::Mod => dyadic::eval_mod(left, right, ctx),
        DyadicOp::Pow => dyadic::eval_pow(left, right, ctx),
        DyadicOp::Index => dyadic::eval_index(left, right, ctx),
        DyadicOp::DotProd => dyadic::eval_dot_product(left, right, ctx),
        DyadicOp::CrossProd => dyadic::eval_cross_product(left, right, ctx),
        _ => Ok(Exp::Dyadic {
            op,
            left: left.into(),
            right: right.into(),
        }),
    }
}

fn eval_pool(op: AssocOp, terms: Vec<Exp>, ctx: &Context) -> EvalResult<Exp> {
    let flattened = fold::flatten_pool(op, terms);
    for term in &flattened {
        let exp_type = term.infer_type(ctx);
        if exp_type != ExpType::Unknown && !op.accepted_types().contains(&exp_type) {
            return Err(EvalError::PoolWrongTermType { op, ty: exp_type });
        }
    }
    let mut new_terms = match op {
        AssocOp::Add => fold::add_fold(flattened, ctx)?,
        AssocOp::Mul => fold::mul_fold(flattened, ctx)?,
        AssocOp::LogicOr => fold::and_or_fold(true, flattened, ctx)?,
        AssocOp::LogicAnd => fold::and_or_fold(false, flattened, ctx)?,
        AssocOp::LogicXor => fold::xor_fold(flattened, ctx)?,
        _ => unimplemented!(),
    };
    Ok(if new_terms.len() == 1 {
        new_terms.pop().unwrap()
    } else {
        Exp::Pool {
            op,
            terms: new_terms,
        }
    })
}
