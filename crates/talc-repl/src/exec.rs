use std::fs::File;
use std::io::{BufReader, prelude::*};
use std::path::Path;

use itertools::Itertools;
use lazy_regex::regex_captures;
use std::str::FromStr;

use talc::ast::{Exp, ProcedureKind};
use talc::context::{ApproxLevel, Context, DEFAULT_CONTEXT};
use talc::display::PrintOptions;
use talc::linalg::Matrix;
use talc::parse::{is_valid_identifier, parse, preparse};
use talc_utils::try_one_to_zero_index;

pub struct ExecContext {
    pub is_repl: bool,
    pub debug_mode: bool,
    pub pretty_printing: bool,
    pub decimal_rationals: Option<bool>,
}

impl Default for ExecContext {
    fn default() -> Self {
        Self {
            is_repl: false,
            debug_mode: false,
            pretty_printing: true,
            decimal_rationals: None,
        }
    }
}

pub fn exec_file(
    path: &Path,
    exec_ctx: &mut ExecContext,
    math_ctx: &mut Context,
) -> std::io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    exec_reader(reader, exec_ctx, math_ctx);
    Ok(())
}

fn exec_reader(reader: BufReader<File>, exec_ctx: &mut ExecContext, math_ctx: &mut Context) {
    for res in reader.lines() {
        let line = res.unwrap();
        if let Some(string) = line.strip_prefix("!") {
            println!("{}", string.trim());
            continue;
        }

        let res = exec_line(&line, exec_ctx, math_ctx);
        match res {
            Some(ExecResult::Error) => return,
            Some(ExecResult::Exit) => return,
            None => (),
        }
    }
}

pub enum ExecResult {
    Exit,
    Error,
}

pub fn exec_line(
    mut line: &str,
    exec_ctx: &mut ExecContext,
    math_ctx: &mut Context,
) -> Option<ExecResult> {
    if line.is_empty() {
        return None;
    }

    if let Some(remainder) = line.strip_prefix(":") {
        let shutdown = eval_command(remainder, exec_ctx, math_ctx);
        return if shutdown {
            Some(ExecResult::Exit)
        } else {
            None
        };
    }

    if let Some((body, _comment)) = line.split_once("#") {
        line = body;
    }

    if let Some((head, tail)) = line.split_once(":=") {
        let parse_result = parse(&preparse(tail));
        let parsed = match parse_result {
            Ok(exp) => exp,
            Err(error) => {
                error.pretty_print(tail);
                return Some(ExecResult::Error);
            }
        };

        // Definitions are evaluated in the empty context, as context may be different when it is
        // used
        let eval_result = parsed.eval(&Context::new());
        let evaluated = match eval_result {
            Ok(exp) => exp,
            Err(error) => {
                println!("Error: {error}");
                return Some(ExecResult::Error);
            }
        };

        let front = head.trim().to_string();

        if let Some((_, id, params_string)) = regex_captures!(r"^(\p{L}+)\((.*)\)$", &front)
            && !id.contains('_')
        {
            if !is_valid_identifier(id) {
                println!("Invalid function name: {id}");
                return Some(ExecResult::Error);
            }
            let params = params_string
                .split(',')
                .map(ToOwned::to_owned)
                .collect_vec();
            for param in &params {
                if !is_valid_identifier(param) {
                    println!("Invalid parameter name: {param}");
                    return Some(ExecResult::Error);
                }
            }
            math_ctx.set_func(id.to_owned(), params, evaluated);
            if exec_ctx.is_repl {
                println!("Defined function {id}");
            }
        } else if let Some((_, name, index)) = regex_captures!(r"(.+)_\(([\d,]+)\)", &front) {
            let one_index = index
                .split(',')
                .map(|part| part.parse::<usize>().unwrap())
                .collect_vec();
            assign_with_index(name, one_index, evaluated, math_ctx);
        } else if let Some((_, name, index)) = regex_captures!(r"(.+)_(\d+)", &front) {
            let one_index = vec![index.parse().unwrap()];
            assign_with_index(name, one_index, evaluated, math_ctx);
        } else {
            if !is_valid_identifier(&front) {
                println!("Invalid variable name: {front}");
                return Some(ExecResult::Error);
            }
            math_ctx.set_var(front.clone(), evaluated);
            if exec_ctx.is_repl {
                println!("Defined variable {front}");
            }
        }
    } else {
        let preparsed = preparse(line);
        if exec_ctx.debug_mode {
            println!("Preparsed: {preparsed}");
        }
        let parse_result = parse(&preparsed);
        let parsed = match parse_result {
            Ok(exp) => exp,
            Err(error) => {
                error.pretty_print(&preparsed);
                return Some(ExecResult::Error);
            }
        };

        let print_options = PrintOptions {
            decimal_rationals: exec_ctx
                .decimal_rationals
                .unwrap_or_else(|| line.contains('.')),
            ..PrintOptions::default()
        };

        if exec_ctx.debug_mode {
            println!("AST: {parsed:?}");
            println!("Displays: {}", parsed.to_string_opts(print_options));
            println!();
        }

        let eval_result = parsed.eval(math_ctx);

        let evaluated = match eval_result {
            Ok(exp) => exp,
            Err(error) => {
                println!("Error: {error}");
                return Some(ExecResult::Error);
            }
        };

        let s = if exec_ctx.pretty_printing {
            evaluated.to_pretty_string_opts(print_options)
        } else {
            evaluated.to_string_opts(print_options)
        };

        println!("{s}");

        if exec_ctx.debug_mode {
            println!();
            println!("Evaluated AST: {evaluated:?}");
        }
    }
    None
}
fn assign_with_index(name: &str, mut one_index: Vec<usize>, exp: Exp, ctx: &mut Context) {
    if !is_valid_identifier(name) {
        println!("Invalid variable name: {name}");
        return;
    }

    let mut mat = match ctx.get_var(name) {
        Some(info) => {
            if let Exp::Matrix(mat) = &info.exp {
                mat.clone()
            } else {
                println!("Cannot assign to index of non-matrix");
                return;
            }
        }
        None => Matrix::filled(Exp::ZERO, (1, 1)),
    };

    if let [i] = one_index[..] {
        if mat.width() == 1 {
            one_index = vec![i, 1];
        } else {
            println!("Cannot index into matrix with single index");
            return;
        }
    }

    let Some(zero_index) = try_one_to_zero_index(&one_index) else {
        println!("Cannot assign to index 0, indexing starts at 1");
        return;
    };

    let new_size = (
        mat.height().max(one_index[0]),
        mat.width().max(one_index[1]),
    );

    mat.resize(new_size, &Exp::ZERO);

    mat[(zero_index[0], zero_index[1])] = exp;

    ctx.set_var(name.to_owned(), mat.into());

    println!("Assigned index {one_index:?} of {name}");
}

fn eval_command(command: &str, exec_ctx: &mut ExecContext, math_ctx: &mut Context) -> bool {
    let Some(kind) = command.chars().next() else {
        return false;
    };
    let args_str = command[kind.len_utf8()..].trim();

    let args = if args_str.is_empty() {
        Vec::with_capacity(0)
    } else {
        command[kind.len_utf8()..].trim().split(' ').collect_vec()
    };
    match kind {
        'q' => return true,
        'h' => {
            for arg in args {
                print_help(arg, math_ctx);
            }
        }
        'l' => list_definitions(&args, math_ctx),
        's' => update_settings(&args, exec_ctx, math_ctx),
        'r' => reset_definitions(&args, math_ctx),
        'e' => {
            if let Some(name) = args.first() {
                let path = Path::new(name);
                let res = exec_file(path, exec_ctx, math_ctx);
                if res.is_err() {
                    println!("Cannot open file");
                }
            }
        }
        _ => println!("Unkown command type {kind}. Ignoring"),
    }

    false
}

fn reset_definitions(vars: &[&str], math_ctx: &mut Context) {
    if vars.is_empty() {
        *math_ctx = DEFAULT_CONTEXT.with_borrow(Clone::clone);
    } else {
        for arg in vars {
            let mut altered = false;
            if let Some(info) = DEFAULT_CONTEXT.with_borrow(|def_ctx| def_ctx.get_var(arg).cloned())
            {
                math_ctx.set_var_info((*arg).to_owned(), info);
                altered = true;
            } else if math_ctx.del_var(arg).is_some() {
                altered = true;
            }

            if let Some(info) =
                DEFAULT_CONTEXT.with_borrow(|def_ctx| def_ctx.get_func(arg).cloned())
            {
                math_ctx.set_func_info((*arg).to_owned(), info);
                altered = true;
            } else if math_ctx.del_func(arg).is_some() {
                altered = true;
            }
            if !altered {
                println!("{arg} is not defined");
            }
        }
    }
}

fn list_definitions(args: &[&str], math_ctx: &Context) {
    let vars = args.is_empty() || args.contains(&"vars") || args.contains(&"all");
    let funcs = args.is_empty() || args.contains(&"funcs") || args.contains(&"all");
    let def_vars = args.contains(&"default_vars") || args.contains(&"all");
    let def_funcs = args.contains(&"default_funcs") || args.contains(&"all");

    if vars || def_vars {
        println!("\nVariables:");
        for (name, info) in math_ctx.variables.iter().sorted_by_key(|(name, _)| *name) {
            let is_default = math_ctx.is_default_var(name);
            if is_default && def_vars {
                println!(" \x1b[90md\x1b[0m {name: <4}=   {}", info.exp);
            } else if !is_default && vars {
                println!("   {name: <4}=   {}", info.exp);
            }
        }
    }

    if funcs || def_funcs {
        println!("\nFunctions:");
        for (name, info) in math_ctx.functions.iter().sorted_by_key(|(name, _)| *name) {
            let is_default = math_ctx.is_default_func(name);
            let full_name = format!("{name}({})", info.params.join(","));
            let exp = info
                .exp
                .clone()
                .map_or("\x1b[90m(built-in)\x1b[0m".to_owned(), |exp| {
                    exp.to_string()
                });
            if is_default && def_funcs {
                println!(" \x1b[90md\x1b[0m {full_name: <10}=   {exp}");
            } else if !is_default && funcs {
                println!("   {full_name: <10}=   {exp}");
            }
        }
    }
}

fn update_settings(args: &[&str], exec_ctx: &mut ExecContext, math_ctx: &mut Context) {
    match args[0] {
        "debug" => exec_ctx.debug_mode = true,
        "normal" => exec_ctx.debug_mode = false,
        "pretty" => exec_ctx.pretty_printing = true,
        "ugly" => exec_ctx.pretty_printing = false,
        "approx" => math_ctx.approx_level = ApproxLevel::SmallFloat,
        "exact" => math_ctx.approx_level = ApproxLevel::None,
        _ => println!("Unknown setting"),
    }
}

fn print_help(arg_string: &str, math_ctx: &Context) {
    if arg_string.is_empty() {
        println!(
            "Talc, a CAS for people with low standards
Created by Jacob Garm, 2024

To use the REPL, type an expression and it will be evaluated.
Use := to define variables and functions.
For a complete list of commands, type :h commands"
        );
    }

    if let Ok(procedure) = ProcedureKind::from_str(arg_string) {
        println!("Printing help for {arg_string} procedure\n");
        println!("Description: {}", procedure.description());
        println!("Signature: ({})", procedure.signature_string());
    }

    if let Some(func_info) = math_ctx.get_func(arg_string) {
        println!("Printing help for {arg_string} function\n");
        println!("Signature: ({})", func_info.params.join(", "));
        if let Some(exp) = &func_info.exp {
            println!("Expression: {exp}");
        } else {
            println!("This function is built-in; it has no expression")
        }
    }
}
