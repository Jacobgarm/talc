use std::collections::HashMap;

use crate::ast::Exp;
use crate::operators::Relation;
use crate::sets::Set;
use malachite::Float;

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    args: Vec<Exp>,
    exp: Option<Exp>,
    domain: Option<Set>,
    codomain: Option<Set>,
    partial_deris: Option<Vec<Exp>>,
    integral: Option<Exp>,
    //float_func: Option<&'static dyn Fn(&[f64]) -> f64>,
    //big_func: Option<&'static dyn Fn(&[Float]) -> Float>,
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    exp: Exp,
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
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            relations: HashMap::new(),
        }
    }

    pub fn set_var(&mut self, name: String, exp: Exp) {
        let var_info = VarInfo::from_exp(exp);
        self.variables.insert(name, var_info);
    }
}

impl Default for Context {
    fn default() -> Self {
        let variables = HashMap::from([("i".to_owned(), VarInfo::from_exp(Exp::ImagUnit))]);
        let functions = HashMap::from([]);
        let relations = HashMap::from([]);
        Self {
            variables,
            functions,
            relations,
        }
    }
}
