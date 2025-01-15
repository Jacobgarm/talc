use itertools::Itertools;
use std::fmt::Display;

use crate::{ast::Exp, typing::ExpType};

#[derive(Debug, Clone)]
pub enum EvalError {
    Unimplemented,

    ProcedureError(String),
    FunctionWrongArgCount {
        name: String,
        expected: usize,
        got: usize,
    },

    NonNaturalIndex {
        index: Exp,
    },
    NonIndexableExp {
        exp: Exp,
    },
    IndexOutOfBounds {
        size: Vec<usize>,
        index: Vec<usize>,
    },
    DivisionByZero,
    ModuloByZero,
    PoolWrongTermType {
        op: super::AssocOp,
        ty: ExpType,
    },
    MatrixScalarSum,
    MatrixSizesIncompatibleAdd {
        left_size: (usize, usize),
        right_size: (usize, usize),
    },
    MatrixSizesIncompatibleMul {
        left_size: (usize, usize),
        right_size: (usize, usize),
    },
    MatrixSizesIncompatibleDot {
        left_size: (usize, usize),
        right_size: (usize, usize),
    },
    MatrixSizesIncompatibleCross {
        left_size: (usize, usize),
        right_size: (usize, usize),
    },
    NonSquareMatrixPower {
        size: (usize, usize),
    },
    RelationChainTooShort {
        len: usize,
    },
    NonnumericDerivative {
        exp: Exp,
    },
}

pub type EvalResult<T> = Result<T, EvalError>;

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use EvalError::*;
        let s = match self {
            Unimplemented => "unimplemented".to_owned(),

            ProcedureError(err) => err.to_owned(),
            FunctionWrongArgCount {
                name,
                expected,
                got,
            } => format!(
                "wrong number of arguments for function {name}, expected {expected} but got {got}"
            ),
            NonNaturalIndex { index } => {
                format!("index is not a natural number or tuple of natural numbers, but {index}")
            }
            NonIndexableExp { exp } => {
                format!("{exp} cannot be indexed")
            }
            IndexOutOfBounds { size, index } => {
                format!(
                    "index out of bounds: index is ({}) but size is ({})",
                    index.iter().join(", "),
                    size.iter().join(", ")
                )
            }

            DivisionByZero => "division by zero".to_owned(),
            ModuloByZero => "modulo by zero".to_owned(),
            PoolWrongTermType { op, ty } => {
                format!(
                    "associative operator '{}' does not support expressions of type {ty}",
                    op.symbol()
                )
            }
            MatrixScalarSum => "cannot add matrices and numbers".to_owned(),
            MatrixSizesIncompatibleAdd {
                left_size,
                right_size,
            } => format!(
                "cannot add matrices of different sizes, left is {}×{} while right is {}×{}",
                left_size.0, left_size.1, right_size.0, right_size.1
            ),
            MatrixSizesIncompatibleMul {
                left_size,
                right_size,
            } => format!(
                "cannot multiply matrices with incompatible sizes, width of left is {} while height of right is {}",
                left_size.1, right_size.0
            ),
            MatrixSizesIncompatibleDot {
                left_size,
                right_size,
            } => {
                if left_size.1 == 1 && right_size.1 == 1 {
                    format!(
                        "cannot take scalar product of vectors of different lengths, left is {} while right is {}",
                        left_size.0, right_size.0
                    )
                } else {
                    format!(
                        "cannot take scalar product of matrices of different sizes, left is {}×{} while right is {}×{}",
                        left_size.0, left_size.1, right_size.0, right_size.1
                    )
                }
            }
            MatrixSizesIncompatibleCross {
                left_size,
                right_size,
            } => {
                if left_size.1 == 1 && right_size.1 == 1 {
                    format!(
                        "cannot take cross product of vectors of lengths, {} and {}",
                        left_size.0, right_size.0
                    )
                } else {
                    format!(
                        "cannot take cross product of matrices, left is {}×{} while right is {}×{}",
                        left_size.0, left_size.1, right_size.0, right_size.1
                    )
                }
            }

            NonSquareMatrixPower { size } => format!(
                "cannot take power of non-square matrix, size is {}×{}",
                size.0, size.1
            ),

            RelationChainTooShort { len } => {
                if *len == 0 {
                    "relation chain is empty".to_owned()
                } else {
                    "relation chain only has a single term".to_owned()
                }
            }
            NonnumericDerivative { exp } => {
                format!("cannot take derivative of non-numeric expression: {exp}")
            }
        };
        write!(f, "{s}")
    }
}
