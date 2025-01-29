use malachite::Float;
use std::collections::HashMap;

use crate::ast::Exp;
use crate::ast::Relation;
use crate::sets::Set;

mod approximators;
mod default;
mod float_functions;
mod functions;

pub use default::DEFAULT_CONTEXT;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FunctionInfo {
    pub params: Vec<String>,
    pub exp: Option<Exp>,
    domain: Option<Set>,
    codomain: Option<Set>,
    pub partial_deris: Vec<Option<Exp>>,
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
