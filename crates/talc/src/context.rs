use itertools::Itertools;
use malachite::Float;
use malachite::num::float::NiceFloat;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::Relation;
use crate::ast::{ComplexNum, Exp, RealNum};
use crate::parse::parse;
use crate::sets::Set;

mod approximators;
mod float_functions;
mod functions;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FunctionInfo {
    pub params: Vec<String>,
    pub exp: Option<Exp>,
    domain: Option<Set>,
    codomain: Option<Set>,
    pub partial_deris: Option<Vec<Exp>>,
    integral: Option<Exp>,
    pub float_func: Option<fn(f64) -> f64>,
    pub vec_float_func: Option<fn(Vec<f64>) -> Vec<f64>>,
    pub rust_func: Option<fn(Vec<Exp>) -> Exp>,
    //big_func: Option<&'static dyn Fn(&[Float]) -> Float>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub variables: HashMap<String, VarInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub relations: HashMap<(Exp, Relation), Exp>,
    pub foil_level: FoilLevel,
    pub approx_level: ApproxLevel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FoilLevel {
    Always,
    SingleAlgebraic,
    Never,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ApproxLevel {
    None,
    BigFloat(u64),
    SmallFloat,
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
            approx_level: ApproxLevel::None,
        }
    }

    pub fn set_var_info(&mut self, name: String, info: VarInfo) {
        self.variables.insert(name, info);
    }

    pub fn set_var(&mut self, name: String, exp: Exp) {
        let var_info = VarInfo::from_exp(exp);
        self.variables.insert(name, var_info);
    }

    pub fn get_var(&self, name: &str) -> Option<&VarInfo> {
        self.variables.get(name)
    }

    pub fn del_var(&mut self, name: &str) -> Option<VarInfo> {
        self.variables.remove(name)
    }

    pub fn set_func_info(&mut self, name: String, info: FunctionInfo) {
        self.functions.insert(name, info);
    }

    pub fn set_func(&mut self, name: String, params: Vec<String>, exp: Exp) {
        let func_info = FunctionInfo {
            params,
            exp: Some(exp),
            ..FunctionInfo::default()
        };
        self.functions.insert(name, func_info);
    }

    pub fn get_func(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.get(name)
    }

    pub fn del_func(&mut self, name: &str) -> Option<FunctionInfo> {
        self.functions.remove(name)
    }

    pub fn is_default_var(&self, name: &str) -> bool {
        DEFAULT_CONTEXT.with_borrow(|def_ctx| self.get_var(name) == def_ctx.get_var(name))
    }

    pub fn is_default_func(&self, name: &str) -> bool {
        DEFAULT_CONTEXT.with_borrow(|def_ctx| self.get_func(name) == def_ctx.get_func(name))
    }
}

impl Default for Context {
    fn default() -> Self {
        use approximators::*;
        use std::f64::consts;

        let simple_constants = [("i", Exp::Complex(ComplexNum::I))];
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
                exp: RealNum::Small(NiceFloat(val)).into(),
                approximator: Some(approx),
            };
            variables.insert(sym.to_owned(), var_info);
        }

        let simple_functions = [
            ("sqrt", &["x"] as &[&str], "x^0.5"),
            ("curt", &["x"], "x^(1/3)"),
            ("exp", &["x"], "e^x"),
            ("log", &["x", "b"], "ln(x)/ln(b)"),
            ("tan", &["x"], "sin(x)/cos(x)"),
            ("csc", &["x"], "1/sin(x)"),
            ("sec", &["x"], "1/cos(x)"),
            ("cot", &["x"], "cos(x)/sin(x)"),
        ];

        let advanced_functions = [
            ("ln", FunctionInfo {
                params: vec!["x".to_owned()],
                float_func: Some(f64::ln as fn(f64) -> f64),
                partial_deris: Some(vec![parse("1/x").unwrap()]),
                ..Default::default()
            }),
            ("sin", FunctionInfo {
                params: vec!["x".to_owned()],
                float_func: Some(f64::sin as fn(f64) -> f64),
                partial_deris: Some(vec![parse("cos(x)").unwrap()]),
                ..Default::default()
            }),
            ("cos", FunctionInfo {
                params: vec!["x".to_owned()],
                float_func: Some(f64::cos as fn(f64) -> f64),
                partial_deris: Some(vec![parse("-sin(x)").unwrap()]),
                ..Default::default()
            }),
            ("ζ", FunctionInfo {
                params: vec!["s".to_owned()],
                float_func: Some(float_functions::riemann_zeta_em),
                ..Default::default()
            }),
            ("Γ", FunctionInfo {
                params: vec!["z".to_owned()],
                partial_deris: Some(vec![parse("Γ(z)*Ψ(z,0)").unwrap()]),
                ..Default::default()
            }),
            ("Ψ", FunctionInfo {
                params: vec!["z".to_owned(), "n".to_owned()],
                ..Default::default()
            }),
        ];

        let mut functions = HashMap::from([]);
        for (name, params, exp_str) in simple_functions {
            let exp = parse(exp_str).expect("parsing error in default function");
            //let exp = Exp::Procedure {
            //    kind: crate::ast::ProcedureKind::EvalDefaultContext,
            //    args: vec![vec![exp]],
            //};

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
            approx_level: ApproxLevel::None,
        }
    }
}
