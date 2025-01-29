use itertools::Itertools;
use malachite::Float;
use malachite::num::float::NiceFloat;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{ComplexNum, Exp, RealNum};
use crate::parse::parse;

use super::*;

thread_local! {
    pub static DEFAULT_CONTEXT: RefCell<Context> = RefCell::new(Context::default());
}

impl Default for Context {
    fn default() -> Self {
        use super::approximators::*;
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
                partial_deris: vec![Some(parse("1/x").unwrap())],
                ..Default::default()
            }),
            ("sin", FunctionInfo {
                params: vec!["x".to_owned()],
                float_func: Some(f64::sin as fn(f64) -> f64),
                partial_deris: vec![Some(parse("cos(x)").unwrap())],
                ..Default::default()
            }),
            ("cos", FunctionInfo {
                params: vec!["x".to_owned()],
                float_func: Some(f64::cos as fn(f64) -> f64),
                partial_deris: vec![Some(parse("-sin(x)").unwrap())],
                ..Default::default()
            }),
            ("ζ", FunctionInfo {
                params: vec!["s".to_owned()],
                float_func: Some(float_functions::riemann_zeta_em),
                ..Default::default()
            }),
            ("Γ", FunctionInfo {
                params: vec!["z".to_owned()],
                partial_deris: vec![Some(parse("Γ(z)*Ψ(z,0)").unwrap())],
                ..Default::default()
            }),
            ("ψ", FunctionInfo {
                params: vec!["z".to_owned(), "n".to_owned()],
                partial_deris: vec![Some(parse("Ψ(z,n+1)").unwrap()), None],
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
