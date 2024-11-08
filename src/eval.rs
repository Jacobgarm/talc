use crate::{ast::Exp, context::Context};

mod fold;

#[derive(Debug, Clone)]
pub enum EvalError {
    DivisionByZero,
}

type EvalResult<T> = Result<T, EvalError>;

impl Exp {
    pub fn eval(&self, ctx: &Context) -> EvalResult<Exp> {
        Ok(match self {
            atom if atom.is_atomic() => atom.clone(),
            _ => self.clone(),
        })
    }
}
