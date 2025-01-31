use std::{char, cmp::Ordering, collections::HashMap, fmt::Display, str::FromStr};

use itertools::Itertools;
use lazy_regex::{regex, regex_captures, regex_is_match, regex_replace_all};
use malachite::{Integer, Rational, num::conversion::traits::FromSciString};
use strum::IntoEnumIterator;
use talc_utils::lower_superscript;

use crate::ast::*;
use operators::{Infix, infix_from_char};

pub fn is_valid_identifier(s: &str) -> bool {
    regex_is_match!(r"^\p{L}[\p{L}0-9]*$", s)
}

pub fn get_unicode_replacement(name: &str) -> Option<char> {
    // Manual overrides
    match name {
        "true" => return Some('⊤'),
        "false" => return Some('⊥'),
        "xor" => return Some('⊻'),
        _ => (),
    }

    let names = name.split('.').collect_vec();
    if let Some(codex::Def::Symbol(sym)) = codex::SYM.get(names[0]) {
        if let Some((_, tail)) = name.split_once('.') {
            match sym {
                codex::Symbol::Single(_) => (),
                codex::Symbol::Multi(chs) => {
                    for (id, ch) in chs {
                        if *id == tail {
                            return Some(*ch);
                        }
                    }
                }
            }
            None
        } else {
            Some(match sym {
                codex::Symbol::Single(ch) => ch,
                codex::Symbol::Multi(chs) => chs[0].1,
            })
        }
    } else {
        None
    }
}

pub fn preparse(subject: &str) -> String {
    // Replace names fore unicode characters with the characters using codex
    let identifier = regex!(r"[a-zA-Z]+(?:\.[a-zA-Z]+)*");
    let mut byte_offset = 0;
    let mut s = subject.to_owned();
    for hit in identifier.find_iter(subject) {
        let front = &subject[..hit.start()];
        let back = &subject[hit.end()..];
        if regex_is_match!(r".*[\w\.]$", front) || regex_is_match!(r"^[\[\w\.].*", back) {
            continue;
        }
        if let Some(replacement) = get_unicode_replacement(hit.as_str()) {
            s.replace_range(
                (hit.start() as isize + byte_offset) as usize
                    ..(hit.end() as isize + byte_offset) as usize,
                &replacement.to_string(),
            );
            byte_offset -= hit.len() as isize - replacement.len_utf8() as isize;
        }
    }

    // Lower prime superscripts
    s = regex_replace_all!(r"'[⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻]+", &s, |hit| {
        lower_superscript(hit)
    })
    .to_string();

    // Superscripts to powers
    s = regex_replace_all!(r"[⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻]+", &s, |hit| format!(
        "^({})",
        lower_superscript(hit)
    ))
    .to_string();

    // Replacement symbols
    let symbols = [
        ("!=", "≠"),
        ("<=>", "⇔"),
        ("=>", "⇒"),
        (">=", "≥"),
        ("<=", "≤"),
        ("~", "≈"),
        ("||", "‖"),
        (" ", ""),
        ("\t", ""),
        ("\n", ""),
    ];

    for (symbol, replacement) in symbols {
        s = s.replace(symbol, replacement);
    }

    //Implicit multiplication

    // Juxtaposed parentheses
    s = s.replace(")(", ")*(");

    // Number next to parentheses
    s = regex_replace_all!(
        r"(?<pre>(?:^|[^\w'])[0-9]+(?:.[0-9]+)?)(?<post>\()",
        &s,
        |_, pre, post| format!("{pre}*{post}"),
    )
    .to_string();

    // Number next to variable
    s = regex_replace_all!(
        r"(?<pre>(?:^|\W)[0-9]+(?:.[0.9]+)?)(?<post>\p{L})",
        &s,
        |_, pre, post| format!("{pre}*{post}"),
    )
    .to_string();

    // Parenthesis next to variable or number
    s = regex_replace_all!(r"(?<pre>\))(?<post>\p{L})", &s, |_, pre, post| format!(
        "{pre}*{post}",
    ),)
    .to_string();

    s
}

#[derive(Debug, Clone)]
pub struct ParseExpError {
    message: &'static str,
    start: usize,
    end: usize,
}

type ParseResult<T> = Result<T, ParseExpError>;

impl Display for ParseExpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseExpError {}

impl ParseExpError {
    pub fn pretty_print(self, subject: &str) {
        println!("Parsing error: {}\n", self.message);
        println!("  {subject}");
        if self.start == self.end {
            let spaces = 1 + subject[..self.start].chars().count();
            println!("{}/\\", " ".repeat(spaces));
        } else {
            let spaces = 2 + subject[..self.start].chars().count();
            let tildes = subject[self.start..self.end].chars().count();
            println!("{}{}", " ".repeat(spaces), "~".repeat(tildes));
        }
    }
}

#[allow(clippy::type_complexity)]
pub fn split_at_least_precedent(subject: &str) -> Option<(Vec<&str>, Vec<(char, Infix)>)> {
    let group_pairs = HashMap::from([
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('|', '|'),
        ('‖', '‖'),
        ('⌊', '⌋'),
        ('⌈', '⌉'),
    ]);

    let mut terms = Vec::new();
    let mut ops = Vec::new();

    let mut group_stack = Vec::new();
    let mut lowest_precedence = u8::MAX;
    let mut next_term_start = 0;

    for (index, c) in subject.char_indices() {
        if group_stack.last().is_some_and(|ender| *ender == c) {
            group_stack.pop();
        } else if let Some(ender) = group_pairs.get(&c) {
            group_stack.push(*ender);
        } else if 0 < index && index < subject.len() && group_stack.is_empty() {
            if let Some(infix) = infix_from_char(c) {
                let prec = infix.precedence();
                match prec.cmp(&lowest_precedence) {
                    Ordering::Less => {
                        terms.clear();
                        ops.clear();
                        lowest_precedence = prec;

                        terms.push(&subject[..index]);
                    }
                    Ordering::Equal => {
                        terms.push(&subject[next_term_start..index]);
                    }
                    Ordering::Greater => continue,
                }
                ops.push((c, infix));
                next_term_start = index + c.len_utf8();
            }
        }
    }

    let tail = &subject[next_term_start..];
    terms.push(tail);

    Some((terms, ops))
}

/// Splits a string at unenclosed ',' and ';' and parses each chunk
pub fn parse_separated(subject: &str, start: usize) -> ParseResult<Vec<Vec<Exp>>> {
    let group_pairs = HashMap::from([
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('|', '|'),
        ('‖', '‖'),
        ('⌊', '⌋'),
        ('⌈', '⌉'),
    ]);

    let mut args = Vec::new();
    let mut current_sub_args = Vec::new();

    let mut group_stack = Vec::new();
    let mut next_term_start = 0;

    for (index, c) in subject.char_indices() {
        if group_stack.last().is_some_and(|ender| *ender == c) {
            group_stack.pop();
        } else if let Some(ender) = group_pairs.get(&c) {
            group_stack.push(*ender);
        } else if 0 < index && index < subject.len() && group_stack.is_empty() && ",;".contains(c) {
            let sub = &subject[next_term_start..index];
            let arg = parse_bounded(sub, start + next_term_start)?;
            current_sub_args.push(arg);
            if c == ';' {
                args.push(current_sub_args);
                current_sub_args = Vec::new();
            }
            next_term_start = index + c.len_utf8();
        }
    }

    if next_term_start < subject.len() {
        let tail = &subject[next_term_start..];
        let arg = parse_bounded(tail, start + next_term_start)?;
        current_sub_args.push(arg);
    }

    if !current_sub_args.is_empty() {
        args.push(current_sub_args);
    }

    Ok(args)
}

pub fn parse_primes(primes_str: &str, num_args: usize, start: usize) -> ParseResult<Vec<usize>> {
    if primes_str.chars().all(|c| c == '\'') {
        match num_args {
            0 => Err(ParseExpError {
                message: "Cannot differentiate function of no arguments",
                start,
                end: start + primes_str.len(),
            }),
            1 => Ok(vec![1; primes_str.len()]),
            2.. => Err(ParseExpError {
                message: "Ambiguous prime on function of multiple variables",
                start,
                end: start + primes_str.len(),
            }),
        }
    } else {
        let mut primes = Vec::new();
        for s in primes_str.split('\'') {
            if s.is_empty() {
                continue;
            }
            let Ok(num) = usize::from_str(s) else {
                return Err(ParseExpError {
                    message: "Unparsable prime index",
                    start,
                    end: start + primes_str.len(),
                });
            };

            if num == 0 {
                return Err(ParseExpError {
                    message: "Prime indices start from 1",
                    start,
                    end: start + primes_str.len(),
                });
            }

            if num > num_args {
                return Err(ParseExpError {
                    message: "Prime index larger than number of arguments",
                    start,
                    end: start + primes_str.len(),
                });
            }

            primes.push(num);
        }
        Ok(primes)
    }
}

pub fn parse_function(
    name: &str,
    primes: &str,
    args_string: &str,
    start: usize,
) -> ParseResult<Exp> {
    let args = parse_separated(args_string, start)?;
    let args = args.into_iter().flatten().collect_vec();

    let primes = if primes.is_empty() {
        Vec::with_capacity(0)
    } else {
        parse_primes(primes, args.len(), start + name.len())?
    };

    Ok(Exp::Function {
        name: name.to_owned(),
        primes,
        args,
    })
}

pub fn parse_procedure(name: &str, args_string: &str, start: usize) -> ParseResult<Exp> {
    let args = parse_separated(args_string, start)?;

    let Ok(kind) = ProcedureKind::from_str(name) else {
        return Err(ParseExpError {
            message: "Unknown procedure",
            start,
            end: start + name.len(),
        });
    };

    Ok(Exp::Procedure { kind, args })
}

fn literal_exp(subject: &str) -> Option<Exp> {
    Some({
        match subject {
            "∞" => Exp::Inf,
            "⊤" => Exp::Bool(true),
            "⊥" => Exp::Bool(false),
            _ => return None,
        }
    })
}

pub fn parse(subject: &str) -> ParseResult<Exp> {
    parse_bounded(subject, 0)
}

fn parse_bounded(subject: &str, start: usize) -> ParseResult<Exp> {
    if subject.is_empty() {
        return Err(ParseExpError {
            message: "Cannot parse empty string",
            start,
            end: start,
        });
    }

    // Dyadic and associtive operators
    if let Some((mut terms, mut ops)) = split_at_least_precedent(subject)
        && !ops.is_empty()
    {
        let mut start_indices = vec![start];

        let mut index = start;
        for (term, (c, _)) in terms.iter().zip(ops.iter()) {
            index += term.len() + c.len_utf8();
            start_indices.push(index);
        }

        let assoc = Infix::precedence_associativity(ops.first().unwrap().1.precedence());

        if assoc == Associativity::Right {
            terms = terms.into_iter().rev().collect_vec();
            ops = ops.into_iter().rev().collect_vec();
            start_indices = start_indices.into_iter().rev().collect_vec();
        }

        let mut terms_iter = terms.into_iter();
        let mut indices_iter = start_indices.into_iter();

        let first_term = terms_iter.next().unwrap();

        let mut exp = parse_bounded(first_term, indices_iter.next().unwrap())?;

        let mut exp_is_first = true;

        for ((term, index), (op_char, infix)) in terms_iter.zip_eq(indices_iter).zip_eq(ops) {
            let mut parsed = parse_bounded(term, index)?;

            match op_char {
                '-' => {
                    parsed = Exp::assoc_combine(operators::AssocOp::Mul, Exp::NEGATIVE_ONE, parsed);
                }
                '/' => {
                    parsed = Exp::Dyadic {
                        op: DyadicOp::Pow,
                        left: parsed.into(),
                        right: Exp::NEGATIVE_ONE.into(),
                    }
                }
                _ => (),
            }

            let (left, right) = match assoc {
                Associativity::Left => (exp, parsed),
                Associativity::Right => (parsed, exp),
            };

            exp = match infix {
                Infix::Dyadic(op) => Exp::Dyadic {
                    op,
                    left: left.into(),
                    right: right.into(),
                },
                Infix::Assoc(op) => Exp::assoc_combine(op, left, right),
                Infix::Relation(rel) => Exp::chain_combine(
                    rel,
                    left,
                    right,
                    assoc.is_left() && !exp_is_first,
                    assoc.is_right() && !exp_is_first,
                ),
            };

            exp_is_first = false;
        }

        return Ok(exp);
    }

    // Functions
    if let Some((_, name, primes, insides)) =
        regex_captures!(r"^(\p{L}[\p{L}0-9]*)(['0-9]*)\((.*)\)$", subject)
    {
        return parse_function(name, primes, insides, start);
    }

    // Procedures
    if let Some((_, name, insides)) = regex_captures!(r"^(\p{L}[\p{L}0-9]*)\[(.*)\]$", subject) {
        return parse_procedure(name, insides, start);
    }

    // Parentheses
    if subject.starts_with('(') && subject.ends_with(')') {
        let chunks = parse_separated(&subject[1..subject.len() - 1], start + 1)?;
        let mut chunks = chunks.into_iter().flatten().collect_vec();
        return Ok(if chunks.len() == 1 {
            chunks.pop().unwrap()
        } else {
            Exp::Tuple(chunks)
        });
    }

    //Literals
    if let Some(exp) = literal_exp(subject) {
        return Ok(exp);
    }

    // Integers
    if regex_is_match!(r"^-?[0-9]+$", subject) {
        let int = Integer::from_str(subject).expect("failed parsing integer");
        return Ok(RealNum::Integer(int).into());
    }

    // Real numbers
    if regex_is_match!(r"^-?[0-9]+\.[0-9]+$", subject) {
        let rat = Rational::from_sci_string(subject).expect("failed real number parsing");
        return Ok(RealNum::Rational(rat).into());
    }

    //Variables
    if regex_is_match!(r"^\p{L}[\p{L}0-9]*$", subject) {
        return Ok(Exp::Var {
            name: subject.to_owned(),
        });
    }

    // Ignore Unary add
    if let Some(sub) = subject.strip_prefix("+")
        && !sub.is_empty()
    {
        return parse(sub);
    }

    // Unary minus parsed as multiplication
    if let Some(sub) = subject.strip_prefix("-")
        && !sub.is_empty()
    {
        let parsed = parse(sub)?;
        return Ok(Exp::assoc_combine(
            operators::AssocOp::Mul,
            Exp::NEGATIVE_ONE,
            parsed,
        ));
    }

    // Unary operators
    for op in UnaryOp::iter() {
        let (pre, post) = op.symbols();
        if let Some(sub) = subject.strip_prefix(pre).and_then(|s| s.strip_suffix(post))
            && !sub.is_empty()
        {
            let parsed = parse_bounded(sub, start + pre.len())?;
            return Ok(Exp::Unary {
                op,
                operand: parsed.into(),
            });
        }
    }

    // If no cases match, we do not know how to parse the expression
    let error = ParseExpError {
        message: "Unparsable expression found",
        start,
        end: start + subject.len(),
    };

    Err(error)
}

impl FromStr for Exp {
    type Err = ParseExpError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let preparsed = preparse(s);
        parse(&preparsed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_succesfully() {
        assert!(parse("2+2").is_ok());
        assert!(parse("2-3").is_ok());
        assert!(parse("9*2").is_ok());
        assert!(parse("4/7").is_ok());
    }

    #[test]
    fn test_associativity() {
        use DyadicOp::*;
        use Exp::*;

        let exp1 = parse("a^b^c");
        assert!(exp1.is_ok());

        assert_eq!(exp1.unwrap(), Dyadic {
            op: Pow,
            left: Var {
                name: "a".to_owned()
            }
            .into(),
            right: Dyadic {
                op: Pow,
                left: Var {
                    name: "b".to_owned()
                }
                .into(),
                right: Var {
                    name: "c".to_owned()
                }
                .into()
            }
            .into()
        });

        let exp2 = parse("a%b%c");
        assert!(exp2.is_ok());

        assert_eq!(exp2.unwrap(), Dyadic {
            op: Mod,
            left: Dyadic {
                op: Mod,
                left: Var {
                    name: "a".to_owned()
                }
                .into(),
                right: Var {
                    name: "b".to_owned()
                }
                .into()
            }
            .into(),
            right: Var {
                name: "c".to_owned()
            }
            .into()
        });
    }
}
