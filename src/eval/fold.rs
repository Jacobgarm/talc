use crate::ast::{Exp, Numeric};
use crate::context::Context;
use crate::linalg::Matrix;

pub fn add_fold(terms: &[Exp], ctx: Context) -> Exp {
    if terms.is_empty() {
        return Exp::ZERO;
    }

    let mut num = Numeric::ZERO_INT;
    let mut mat: Option<Matrix> = None;

    for term in terms {
        match term {
            _ => (),
        }
    }
    num.into()
}
