use itertools::Itertools;
use malachite::num::float::NiceFloat;
use malachite::Float;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{Exp, Numeric};
use crate::operators::Relation;
use crate::parse::parse;
use crate::sets::Set;

mod approximators;

#[derive(Debug, Clone, Default)]
pub struct FunctionInfo {
    pub params: Vec<String>,
    pub exp: Option<Exp>,
    domain: Option<Set>,
    codomain: Option<Set>,
    partial_deris: Option<Vec<Exp>>,
    integral: Option<Exp>,
    float_func: Option<fn(f64) -> f64>,
    //big_func: Option<&'static dyn Fn(&[Float]) -> Float>,
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub exp: Exp,
    approximator: Option<fn(u32) -> Float>,
}

impl VarInfo {
    fn from_exp(exp: Exp) -> Self {
        Self {
            exp,
            approximator: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    variables: HashMap<String, VarInfo>,
    functions: HashMap<String, FunctionInfo>,
    relations: HashMap<(Exp, Relation), Exp>,
    foil_level: FoilLevel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FoilLevel {
    Always,
    SingleAlgebraic,
    Never,
}

thread_local! {
    pub static DEFAULT_CONTEXT: RefCell<Context> = RefCell::new(Context::default());
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            relations: HashMap::new(),
            foil_level: FoilLevel::SingleAlgebraic,
        }
    }

    pub fn set_var(&mut self, name: String, exp: Exp) {
        let var_info = VarInfo::from_exp(exp);
        self.variables.insert(name, var_info);
    }

    pub fn get_var(&self, name: &str) -> Option<&VarInfo> {
        self.variables.get(name)
    }

    pub fn set_function(&mut self, name: String, params: Vec<String>, exp: Exp) {
        let func_info = FunctionInfo {
            params,
            exp: Some(exp),
            ..FunctionInfo::default()
        };
        self.functions.insert(name, func_info);
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.get(name)
    }
}

impl Default for Context {
    fn default() -> Self {
        use approximators::*;
        use std::f64::consts;

        let simple_constants = [("i", Exp::ImagUnit)];
        let transcendental_constants = [
            ("π", consts::PI, approx_pi as fn(u32) -> Float),
            ("τ", consts::TAU, approx_tau as fn(u32) -> Float),
            ("e", consts::E, approx_e as fn(u32) -> Float),
            ("γ", consts::EGAMMA, approx_e_gamma as fn(u32) -> Float),
        ];
        let mut variables = HashMap::new();

        for (sym, exp) in simple_constants {
            variables.insert(sym.to_owned(), VarInfo::from_exp(exp));
        }

        for (sym, val, approx) in transcendental_constants {
            let var_info = VarInfo {
                exp: Exp::Number(Numeric::Small(NiceFloat(val))),
                approximator: Some(approx),
            };
            variables.insert(sym.to_owned(), var_info);
        }

        let simple_functions = [
            ("log", &["x", "b"] as &[&str], "ln(x)/ln(b)"),
            ("double", &["x"], "2*x"),
        ];

        let advanced_functions = [
            (
                "ln",
                FunctionInfo {
                    params: vec!["x".to_owned()],
                    float_func: Some(f64::sin as fn(f64) -> f64),
                    partial_deris: Some(vec![parse("1/x").unwrap()]),
                    ..Default::default()
                },
            ),
            (
                "sin",
                FunctionInfo {
                    params: vec!["x".to_owned()],
                    float_func: Some(f64::sin as fn(f64) -> f64),
                    partial_deris: Some(vec![parse("cos(x)").unwrap()]),
                    ..Default::default()
                },
            ),
        ];

        let mut functions = HashMap::from([]);
        for (name, params, exp_str) in simple_functions {
            let exp = parse(exp_str).expect("parsing error in default function");
            let exp = Exp::Procedure {
                kind: crate::ast::ProcedureKind::EvalDefaultContext,
                args: vec![vec![exp]],
            };

            let func_info = FunctionInfo {
                params: params.iter().map(|p| (*p).to_owned()).collect_vec(),
                exp: Some(exp),
                ..Default::default()
            };
            functions.insert(name.to_owned(), func_info);
        }

        for (name, info) in advanced_functions {
            functions.insert(name.to_owned(), info);
        }

        let relations = HashMap::from([]);
        Self {
            variables,
            functions,
            relations,
            foil_level: FoilLevel::SingleAlgebraic,
        }
    }
}
