use itertools::Itertools;
use lazy_regex::regex_is_match;
use malachite::{
    Natural,
    num::{arithmetic::traits::IsPowerOf2, basic::traits::One},
};
use std::{borrow::Borrow, fmt::Display};
use talc_utils::raise_superscript;

use crate::ast::{AssocOp, ComplexNum, DyadicOp, Exp, ProcedureKind, RealNum};

#[derive(Debug, Clone, Copy)]
pub struct PrintOptions {
    pub decimal_rationals: bool,
    pub unicode_exponents: bool,
}

impl Default for PrintOptions {
    fn default() -> Self {
        Self {
            decimal_rationals: false,
            unicode_exponents: true,
        }
    }
}

fn is_power_of_2_and_5(num: &Natural) -> bool {
    let mut reduced = num.clone();
    while &reduced % Natural::from(5u64) == 0 {
        reduced /= Natural::from(5u64);
    }
    reduced.is_power_of_2()
}

impl RealNum {
    fn to_string_opts(&self, opts: PrintOptions) -> String {
        match self {
            RealNum::Integer(int) => int.to_string(),
            RealNum::Rational(frac) => {
                if opts.decimal_rationals && is_power_of_2_and_5(frac.denominator_ref()) {
                    let (before, after) = frac.to_digits(&Natural::from(10u64));
                    let after = after.into_vecs().0;
                    let mut s = before.into_iter().rev().join("");
                    if s.is_empty() {
                        s.push('0');
                    }
                    if !after.is_empty() {
                        s.push('.');
                        s.push_str(&after.into_iter().join(""));
                    }

                    s
                } else {
                    frac.to_string()
                }
            }
            RealNum::Small(float) => float.to_string(),
            RealNum::Big(float) => float.to_string(),
        }
    }
}

impl Display for RealNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let opts = PrintOptions::default();
        f.write_str(&self.to_string_opts(opts))
    }
}

impl ComplexNum {
    fn to_string_opts(&self, opts: PrintOptions) -> String {
        let left = if self.real.is_exact_zero() {
            "".to_owned()
        } else {
            self.real.to_string_opts(opts)
        };
        let sign = if self.imag.is_negative() { "" } else { "+" };

        let right = if self.imag.is_exact_one() {
            "i".to_owned()
        } else if (-self.imag.clone()).is_exact_one() {
            "-i".to_owned()
        } else {
            format!("{}i", self.imag.to_string_opts(opts))
        };
        if left.is_empty() {
            right
        } else {
            format!("{left}{sign}{right}")
        }
    }
}

impl Display for ComplexNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let opts = PrintOptions::default();
        f.write_str(&self.to_string_opts(opts))
    }
}

fn join_nested(args: &[Vec<Exp>], opts: PrintOptions) -> String {
    args.iter()
        .map(|sub| sub.iter().map(|exp| exp.to_string_opts(opts)).join(", "))
        .join("; ")
}

fn is_enclosed(exp: &Exp, opts: PrintOptions) -> bool {
    match exp {
        Exp::Unary { op, .. } => op.is_wrapping(),
        // RelationChains and pools with single terms are still considered unenclosed, to make them
        // more obvious
        Exp::Dyadic { .. } | Exp::RelationChain { .. } | Exp::Pool { .. } => false,
        Exp::Real(RealNum::Rational(rat)) => {
            rat.denominator_ref() == &Natural::ONE || opts.decimal_rationals
        }
        Exp::Complex(num) => {
            if num.is_real() {
                matches!(
                    num.real,
                    RealNum::Integer(..) | RealNum::Small(..) | RealNum::Big(..)
                )
            } else if num.is_imag() {
                num.imag.is_one()
            } else {
                false
            }
        }

        _ => true,
    }
}

fn infix_precedence(exp: &Exp, opts: PrintOptions) -> Option<u8> {
    match exp {
        Exp::Real(RealNum::Rational(rat)) => {
            if rat.denominator_ref() == &Natural::ONE || opts.decimal_rationals {
                None
            } else {
                Some(AssocOp::Mul.precedence())
            }
        }
        Exp::Complex(num) => {
            if num.is_real() {
                if matches!(num.real, RealNum::Rational(..)) {
                    Some(AssocOp::Mul.precedence())
                } else {
                    None
                }
            } else if num.is_imag() && num.imag.is_one() {
                None
            } else {
                Some(AssocOp::Mul.precedence())
            }
        }
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
        let opts = PrintOptions::default();
        f.write_str(&self.to_string_opts(opts))
    }
}

impl Exp {
    pub fn to_string_opts(&self, opts: PrintOptions) -> String {
        use Exp::*;
        match self {
            Real(num) => num.to_string_opts(opts),
            Complex(num) => num.to_string_opts(opts),
            Bool(true) => "⊤".to_owned(),
            Bool(false) => "⊥".to_owned(),
            Inf => "∞".to_owned(),
            Var { name } => name.to_owned(),
            Unary { op, operand } => {
                let (pre, post) = op.symbols();

                format!(
                    "{pre}{}{post}",
                    wrap_if(&operand.to_string_opts(opts), !is_enclosed(operand, opts))
                )
            }

            Dyadic { op, left, right } => {
                let prec = op.precedence();

                #[allow(clippy::needless_bool)]
                let wrap_left = if is_enclosed(left, opts) {
                    false
                } else if let Real(val) = left.borrow()
                    && val.is_negative()
                {
                    true
                } else if infix_precedence(left, opts).is_some_and(|sub_prec| sub_prec <= prec) {
                    true
                } else {
                    false
                };

                let left_str = wrap_if(&left.to_string_opts(opts), wrap_left);

                let right_str = right.to_string_opts(opts);

                if opts.unicode_exponents
                    && *op == DyadicOp::Pow
                    && regex_is_match!(r"^[-0123456789]+$", &right_str)
                {
                    let exponent_str = raise_superscript(&right_str);
                    return format!("{left_str}{exponent_str}");
                }

                let wrap_right = infix_precedence(right, opts)
                    .is_some_and(|sub_prec| sub_prec <= prec)
                    || right_str.starts_with('-');

                let right_str = wrap_if(&right_str, wrap_right);

                let tight = matches!(op, DyadicOp::Pow | DyadicOp::Index);

                if tight {
                    format!("{left_str}{}{right_str}", op.symbol())
                } else {
                    format!("{left_str} {} {right_str}", op.symbol())
                }
            }

            Pool { op, terms } => {
                let prec = op.precedence();

                let mut terms_iter = terms.iter();

                let mut s = match terms_iter.next() {
                    None => "".to_owned(),
                    Some(num) if *num == Exp::NEGATIVE_ONE && *op == AssocOp::Mul => "-".to_owned(),
                    Some(exp) => wrap_if(
                        &exp.to_string_opts(opts),
                        infix_precedence(exp, opts).is_some_and(|sub_prec| sub_prec <= prec),
                    ),
                };

                for term in terms_iter {
                    let term_str = wrap_if(
                        &term.to_string_opts(opts),
                        infix_precedence(term, opts).is_some_and(|sub_prec| sub_prec <= prec),
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
                            &left.to_string_opts(opts),
                            infix_precedence(left, opts).is_some_and(|sub_prec| sub_prec <= prec),
                        );
                        format!(" / {sub_term_str}")
                    } else if s == "-" && *op == AssocOp::Mul {
                        term_str
                    } else {
                        format!(" {} {}", op.symbol(), term_str)
                    };

                    s.push_str(&prefixed_str);
                }

                s
            }

            RelationChain { rels, terms } => {
                if terms.is_empty() {
                    "".to_owned()
                } else if rels.is_empty() {
                    terms[0].to_string_opts(opts)
                } else {
                    let prec = rels[0].precedence();

                    let mut term_strs = terms.iter().map(|term| {
                        wrap_if(
                            &term.to_string_opts(opts),
                            infix_precedence(term, opts).is_some_and(|sub_prec| sub_prec <= prec),
                        )
                    });

                    let mut s = term_strs.next().unwrap();

                    for (rel, term_str) in rels.iter().zip_eq(term_strs) {
                        s.push_str(&format!(" {} {}", rel.symbol(), term_str));
                    }
                    s
                }
            }

            Function { name, primes, args } => {
                let args_str = args.iter().map(|arg| arg.to_string_opts(opts)).join(", ");
                let primes_str = if args.len() == 1 {
                    "'".repeat(primes.len())
                } else {
                    primes
                        .iter()
                        .map(|index| format!("'{}", raise_superscript(&index.to_string())))
                        .join("")
                };
                format!("{name}{primes_str}({args_str})")
            }
            Procedure { kind, args } => {
                let name = kind.to_string();
                let args_str = join_nested(args, opts);
                format!("{name}[{args_str}]")
            }
            Tuple(entries) => {
                let entries_str = entries
                    .iter()
                    .map(|arg| arg.to_string_opts(opts))
                    .join(", ");
                format!("({entries_str})")
            }
            Matrix(mat) => {
                if mat.width() == 1 && mat.height() > 1 {
                    let entries_str = mat
                        .rows_ref()
                        .iter()
                        .map(|row| row[0].to_string_opts(opts))
                        .join(", ");
                    let name = ProcedureKind::ColumnVector.to_string();
                    format!("{name}[{entries_str}]")
                } else if mat.height() == 1 && mat.width() > 1 {
                    let entries_str = mat.rows_ref()[0]
                        .iter()
                        .map(|entry| entry.to_string_opts(opts))
                        .join(", ");
                    let name = ProcedureKind::RowVector.to_string();
                    format!("{name}[{entries_str}]")
                } else {
                    let entries_str = join_nested(mat.rows_ref(), opts);
                    let name = ProcedureKind::Matrix.to_string();
                    format!("{name}[{entries_str}]")
                }
            }
        }
    }

    pub fn to_pretty_string_opts(&self, opts: PrintOptions) -> String {
        match self {
            Exp::Matrix(mat) => {
                let rows = mat.rows_ref();
                let height = mat.height();
                let width = mat.width();
                let mut rows_str: Vec<Vec<String>> = Vec::new();
                let mut column_widths: Vec<usize> = vec![1; width];
                for row in 0..height {
                    rows_str.push(Vec::new());
                    for (col, width) in column_widths.iter_mut().enumerate() {
                        let s = rows[row][col].to_string_opts(opts);
                        *width = *width.max(&mut s.len());
                        rows_str[row].push(s);
                    }
                }
                let mut s = String::new();
                for row in rows_str {
                    s.push('│');
                    for (entry, width) in row.iter().zip_eq(column_widths.iter()) {
                        s.push_str(&format!(" {entry:^width$} "));
                    }
                    s.push('│');
                    s.push('\n');
                }
                s
            }
            exp => exp.to_string_opts(opts),
        }
    }
}
