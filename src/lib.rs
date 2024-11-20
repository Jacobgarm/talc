#![feature(let_chains)]
#![feature(more_float_constants)]
#![feature(box_patterns)]
#![allow(dead_code, clippy::if_same_then_else)]

pub mod ast;
pub mod context;
pub mod linalg;
pub mod utils;
pub use ast::operators;
pub mod display;
pub mod eval;
pub mod parse;
pub mod sets;
pub mod typing;
