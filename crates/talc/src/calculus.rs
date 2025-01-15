mod differentiation;
pub use differentiation::{derivative, expand_primed_function};

mod integration;

mod vector;
pub use vector::{gradient, hessian, hessian_symmetric};
