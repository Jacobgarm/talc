use crate::{ast::Exp, context::Context};

#[derive(Debug, Clone)]
pub enum EvalError {
    DivisionByZero,
}

type EvalResult<T> = Result<T, EvalError>;

impl Exp {
    pub fn eval(&self, ctx: &Context) -> EvalResult<Exp> {
        Ok(self.clone())
    }
}
