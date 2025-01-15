use malachite::num::float::NiceFloat;

use crate::{
    ast::{Exp, RealNum},
    context::{ApproxLevel, Context},
    eval::EvalResult,
};

pub struct RealFunction {
    pub exp: Exp,
    vars: Vec<String>,
    ctx: Context,
}

impl RealFunction {
    pub fn new(exp: &Exp, vars: Vec<String>, ctx: &Context) -> EvalResult<Self> {
        let mut new_ctx = ctx.clone();
        new_ctx.approx_level = ApproxLevel::SmallFloat;
        Ok(Self {
            exp: exp.approximate(None, ctx)?,
            vars,
            ctx: new_ctx,
        })
    }

    pub fn eval_point(&mut self, point: &[f64]) -> EvalResult<f64> {
        for (var, val) in self.vars.iter().zip(point.iter()) {
            self.ctx
                .set_var(var.to_string(), RealNum::Small(NiceFloat(*val)).into());
        }
        let res = self.exp.eval(&self.ctx)?;
        if let Exp::Real(RealNum::Small(NiceFloat(num))) = res {
            Ok(num)
        } else {
            panic!("function did not return float")
        }
    }

    pub fn eval_points(&mut self, vals: &[&[f64]]) -> EvalResult<Vec<f64>> {
        let mut outs = Vec::with_capacity(vals.len());
        for vec in vals {
            for (var, val) in self.vars.iter().zip(vec.iter()) {
                self.ctx
                    .set_var(var.to_string(), RealNum::Small(NiceFloat(*val)).into());
            }
            let res = self.exp.eval(&self.ctx)?;
            if let Exp::Real(RealNum::Small(NiceFloat(num))) = res {
                outs.push(num);
            } else {
                panic!("function did not return float")
            }
        }
        Ok(outs)
    }
}
