use std::collections::HashMap;

use itertools::Itertools;

use crate::{
    ast::{AssocOp, DyadicOp, Exp},
    context::Context,
    eval::{EvalError, EvalResult},
    typing::ExpType,
};

pub fn derivative(exp: &Exp, diff_exp: &Exp, ctx: &Context) -> EvalResult<Exp> {
    use Exp::*;
    let ty = exp.infer_type(ctx);
    if !matches!(ty, ExpType::Unknown | ExpType::Numeric | ExpType::Matrix) {
        return Err(EvalError::NonnumericDerivative { exp: exp.clone() });
    } else if exp == diff_exp {
        return Ok(Exp::ONE);
    } else if (ty == ExpType::Numeric || ty == ExpType::Unknown) && !exp.depends_on(diff_exp, ctx) {
        return Ok(Exp::ZERO);
    }
    match exp {
        Var { name } => {
            if let Some(info) = ctx.get_var(name) {
                derivative(&info.exp, diff_exp, ctx)
            } else {
                Ok(Exp::ZERO)
            }
        }
        Dyadic {
            op: DyadicOp::Pow,
            box left,
            box right,
        } => {
            let right_depends = right.depends_on(diff_exp, ctx);
            let left_depends = left.depends_on(diff_exp, ctx);
            if !right_depends {
                let dleft = derivative(left, diff_exp, ctx)?;
                Ok(right.clone() * left.clone().pow(right.clone() - Exp::ONE) * dleft)
            } else if !left_depends {
                let dright = derivative(right, diff_exp, ctx)?;
                let lnleft = Function {
                    name: "ln".to_owned(),
                    primes: Vec::new(),
                    args: vec![left.clone()],
                };
                Ok(lnleft * exp.clone() * dright)
            } else {
                let dleft = derivative(left, diff_exp, ctx)?;
                let dright = derivative(right, diff_exp, ctx)?;
                let lnleft = Function {
                    name: "ln".to_owned(),
                    primes: Vec::new(),
                    args: vec![left.clone()],
                };
                Ok(left.clone().pow(right.clone() - Exp::ONE)
                    * (right.clone() * dleft + left.clone() * lnleft * dright))
            }
        }
        Pool {
            op: AssocOp::Add,
            terms,
        } => Ok(Pool {
            op: AssocOp::Add,
            terms: terms
                .iter()
                .map(|exp| derivative(exp, diff_exp, ctx))
                .try_collect()?,
        }),
        Pool {
            op: AssocOp::Mul,
            terms,
        } => product_rule(terms, diff_exp, ctx),

        Function { name, primes, args } => chain_rule(name, primes, args, diff_exp, ctx),

        Matrix(mat) => Ok(Matrix(
            mat.try_map(|entry| derivative(entry, diff_exp, ctx))?,
        )),

        _ => {
            println!("{exp}      {diff_exp}");
            dbg!(exp);
            unreachable!()
        }
    }
}

fn product_rule(terms: &[Exp], diff_exp: &Exp, ctx: &Context) -> EvalResult<Exp> {
    let mut add_terms = Vec::with_capacity(terms.len());
    for (i, term) in terms.iter().enumerate() {
        let mut prod_terms = Vec::with_capacity(terms.len());
        for (j, other_term) in terms.iter().enumerate() {
            if i == j {
                prod_terms.push(derivative(term, diff_exp, ctx)?);
            } else {
                prod_terms.push(other_term.clone());
            }
        }

        add_terms.push(Exp::Pool {
            op: AssocOp::Mul,
            terms: prod_terms,
        });
    }

    Ok(Exp::Pool {
        op: AssocOp::Add,
        terms: add_terms,
    })
}

fn chain_rule(
    name: &str,
    primes: &[usize],
    args: &[Exp],
    diff_exp: &Exp,
    ctx: &Context,
) -> EvalResult<Exp> {
    // Unknown -> Primes
    // Known but not -> primes
    // Known derivatives -> Expand
    // Known exp -> compute and expand
    let dargs: Vec<Exp> = args
        .iter()
        .map(|arg| derivative(arg, diff_exp, ctx))
        .try_collect()?;

    let mut partials = Vec::with_capacity(args.len());

    if primes.is_empty()
        && let Some(func_info) = ctx.get_func(name)
    {
        //if !primes.is_empty() {
        //    let exp = expand_primed_function(name, primes, ctx)?.unwrap();
        //}
        let mut subs = HashMap::new();
        for (param, arg) in func_info.params.iter().zip_eq(args) {
            subs.insert(
                Exp::Var {
                    name: param.clone(),
                },
                arg.clone(),
            );
        }

        if let Some(deris) = &func_info.partial_deris {
            for deri in deris {
                partials.push(deri.substitute(&subs));
            }
        } else if let Some(func_exp) = &func_info.exp {
            for param in &func_info.params {
                let diff_exp = Exp::Var {
                    name: param.clone(),
                };
                let new_exp = derivative(func_exp, &diff_exp, ctx)?;
                partials.push(new_exp.substitute(&subs));
            }
        } else {
            unreachable!("function has neither expression or partial derivatives");
        }
    } else {
        for i in 1..=args.len() {
            let mut new_primes = primes.to_vec();
            new_primes.push(i);
            let partial = Exp::Function {
                name: name.to_owned(),
                primes: new_primes,
                args: args.to_vec(),
            };
            partials.push(partial);
        }
    }

    let mut new_terms: Vec<Exp> = partials
        .into_iter()
        .zip_eq(dargs)
        .map(|(a, b)| a * b)
        .collect();

    Ok(if new_terms.len() == 1 {
        new_terms.pop().unwrap()
    } else {
        Exp::Pool {
            op: AssocOp::Add,
            terms: new_terms,
        }
    })
}

pub fn expand_primed_function(
    name: &str,
    primes: &[usize],
    ctx: &Context,
) -> EvalResult<Option<Exp>> {
    let Some(info) = ctx.get_func(name) else {
        return Ok(None);
    };

    let mut exp = Exp::Function {
        name: name.to_owned(),
        primes: Vec::new(),
        args: info
            .params
            .iter()
            .map(|s| Exp::Var { name: s.clone() })
            .collect(),
    };

    for prime in primes {
        let diff_exp = Exp::Var {
            name: info.params[prime - 1].clone(),
        };
        exp = derivative(&exp, &diff_exp, ctx)?;
    }
    Ok(Some(exp))
}
