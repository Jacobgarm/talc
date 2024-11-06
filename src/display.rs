use std::{borrow::Borrow, fmt::Display};

use itertools::Itertools;

use crate::ast::{AssocOp, DyadicOp, Exp, Numeric};

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Numeric::Integer(int) => int.to_string(),
            Numeric::Rational(frac) => frac.to_string(),
            Numeric::Small(float) => float.to_string(),
            Numeric::Big(float) => float.to_string(),
        };
        write!(f, "{s}")
    }
}

fn join_nested(args: &[Vec<Exp>]) -> String {
    args.iter().map(|sub| sub.iter().join(", ")).join("; ")
}

fn is_enclosed(exp: &Exp) -> bool {
    match exp {
        Exp::Unary { op, .. } => op.is_wrapping(),
        // RelationChains and pools with single terms are still considered unenclosed, to make them
        // more obvious
        Exp::Dyadic { .. } | Exp::RelationChain { .. } | Exp::Pool { .. } => false,

        _ => true,
    }
}

fn infix_precedence(exp: &Exp) -> Option<u8> {
    match exp {
        Exp::Dyadic { op, .. } => Some(op.precedence()),
        Exp::RelationChain { rels, .. } => rels.first().map(|rel| rel.precedence()),
        Exp::Pool { op, .. } => Some(op.precedence()),
        _ => None,
    }
}

fn wrap_if(s: &str, wrap: bool) -> String {
    if wrap {
        format!("({s})")
    } else {
        s.to_string()
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Exp::*;
        let s = match self {
            Number(num) => &num.to_string(),
            Bool(true) => "⊤",
            Bool(false) => "⊥",
            ImagUnit => "i",
            Inf => "∞",
            Var { name } => name,
            Unary { op, operand } => {
                let (pre, post) = op.symbols();

                &format!(
                    "{pre}{}{post}",
                    wrap_if(&operand.to_string(), !is_enclosed(operand))
                )
            }

            Dyadic { op, left, right } => {
                let prec = op.precedence();

                #[allow(clippy::needless_bool)]
                let wrap_left = if is_enclosed(left) {
                    false
                } else if let Number(val) = left.borrow()
                    && val.is_negative()
                {
                    true
                } else if infix_precedence(left).is_some_and(|sub_prec| sub_prec <= prec) {
                    true
                } else {
                    false
                };

                let left_str = wrap_if(&left.to_string(), wrap_left);

                let wrap_right = infix_precedence(right).is_some_and(|sub_prec| sub_prec <= prec)
                    || right.to_string().starts_with("-");

                let right_str = wrap_if(&right.to_string(), wrap_right);

                let tight = *op == DyadicOp::Pow;

                if tight {
                    &format!("{left_str}{}{right_str}", op.symbol())
                } else {
                    &format!("{left_str} {} {right_str}", op.symbol())
                }
            }

            Pool { op, terms } => {
                let prec = op.precedence();

                let mut terms_iter = terms.iter();

                let mut s = match terms_iter.next() {
                    None => "".to_owned(),
                    Some(num) if *num == Exp::NEGATIVE_ONE && *op == AssocOp::Mul => "-".to_owned(),
                    Some(exp) => wrap_if(
                        &exp.to_string(),
                        infix_precedence(exp).is_some_and(|sub_prec| sub_prec <= prec),
                    ),
                };

                for term in terms_iter {
                    let term_str = wrap_if(
                        &term.to_string(),
                        infix_precedence(term).is_some_and(|sub_prec| sub_prec <= prec),
                    );

                    let prefixed_str = if let Some(rest) = term_str.strip_prefix("-")
                        && *op == AssocOp::Add
                    {
                        format!(" - {rest}")
                    } else if let Dyadic {
                        op: DyadicOp::Pow,
                        left,
                        right,
                    } = term
                        && **right == Exp::NEGATIVE_ONE
                        && *op == AssocOp::Mul
                    {
                        let sub_term_str = wrap_if(
                            &left.to_string(),
                            infix_precedence(left).is_some_and(|sub_prec| sub_prec <= prec),
                        );
                        format!(" / {sub_term_str}")
                    } else if s == "-" && *op == AssocOp::Mul {
                        term_str
                    } else {
                        format!(" {} {}", op.symbol(), term_str)
                    };

                    s.push_str(&prefixed_str);
                }

                &s.to_string()
            }

            RelationChain { rels, terms } => {
                if terms.is_empty() {
                    ""
                } else if rels.is_empty() {
                    &terms[0].to_string()
                } else {
                    let prec = rels[0].precedence();

                    let mut term_strs = terms.iter().map(|term| match infix_precedence(term) {
                        Some(sub_prec) if sub_prec <= prec => format!("({term})"),
                        _ => term.to_string(),
                    });

                    let mut s = term_strs.next().unwrap();

                    for (rel, term_str) in rels.iter().zip_eq(term_strs) {
                        s.push_str(&format!(" {} {}", rel.symbol(), term_str));
                    }
                    &s.to_string()
                }
            }

            Function { name, primes, args } => {
                let args_str = args.iter().join(", ");
                let primes_str = if args.len() == 1 {
                    "'".repeat(primes.len())
                } else {
                    primes.iter().map(|index| format!("'{index}")).join("")
                };
                &format!("{name}{primes_str}({args_str})")
            }
            Procedure { kind, args } => {
                let name = kind.to_string();
                let args_str = join_nested(args);
                &format!("{name}[{args_str}]")
            }
            Matrix(mat) => {
                let entries_str = join_nested(mat.rows());
                &format!("mat[{entries_str}]")
            }
        };
        write!(f, "{s}")
    }
}

pub fn pretty_print(exp: &Exp) {
    match exp {
        Exp::Matrix(mat) => {
            let rows = mat.rows();
            let height = mat.height();
            let width = mat.width();
            let mut rows_str: Vec<Vec<String>> = Vec::new();
            let mut column_widths: Vec<usize> = vec![1; width];
            for row in 0..height {
                rows_str.push(Vec::new());
                for (col, width) in column_widths.iter_mut().enumerate() {
                    let s = rows[row][col].to_string();
                    *width = *width.max(&mut s.len());
                    rows_str[row].push(s);
                }
            }
            for row in rows_str {
                print!("│");
                for (entry, width) in row.iter().zip_eq(column_widths.iter()) {
                    print!(" {:^w$} ", entry, w = width);
                }
                println!("│");
            }
        }
        exp => println!("{exp}"),
    }
}
