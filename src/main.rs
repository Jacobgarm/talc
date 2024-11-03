use std::str::FromStr;

use ast::ProcedureKind;
use context::Context;
use itertools::Itertools;
use parse::parse;
use rustyline::{config::Configurer, error::ReadlineError};
use talc::*;

struct REPLContext {
    debug_mode: bool,
    pretty_printing: bool,
}

impl Default for REPLContext {
    fn default() -> Self {
        Self {
            debug_mode: true,
            pretty_printing: true,
        }
    }
}

fn repl(mut eval_context: Context) {
    println!("\nStarting Talc REPL...\n");

    let mut repl_context = REPLContext::default();

    let mut editor = rustyline::DefaultEditor::new().unwrap();

    editor.set_auto_add_history(true);

    if editor.load_history("talc_history.txt").is_err() {
        println!("No history file found. Creating empty file \n");
    }

    loop {
        let read_result = editor.readline("::> ");

        let line = match read_result {
            Ok(line) => line,
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Encountered error while reading input: {err}");
                continue;
            }
        };

        if let Some(remainder) = line.strip_prefix(":") {
            let shutdown = eval_command(remainder, &mut eval_context, &mut repl_context);
            if shutdown {
                println!("Exiting...");
                break;
            }
        } else if let Some((head, tail)) = line.split_once(":=") {
            let var_name = head.trim().to_owned();
            let parse_result = parse(tail);
            match parse_result {
                Ok(exp) => eval_context.set_var(var_name, exp),
                Err(error) => error.pretty_print(tail),
            }
        } else {
            let preparsed = parse::preparse(line);
            dbg!(&preparsed);
            dbg!(parse(&preparsed));
        }
    }

    editor.save_history("talc_history.txt").unwrap();
}

fn eval_command(command: &str, eval_context: &mut Context, repl_context: &mut REPLContext) -> bool {
    let Some(kind) = command.chars().next() else {
        return false;
    };
    let args = command[kind.len_utf8()..].split(' ').collect_vec();
    match kind {
        'q' => return true,
        'h' => {
            for arg in args {
                print_help(arg)
            }
        }
        _ => println!("Unkown command type {kind}. Ignoring"),
    }

    false
}

fn print_help(arg_string: &str) {
    if arg_string.is_empty() {
        println!(
            "Talc, a CAS for people with low standards
Created by Jacob Garm, 2024

To use the REPL, type an expression and it will be evaluated.
Use := to define variables and functions.
For a complete list of commands, type :h commands"
        )
    }

    if let Ok(procedure) = ProcedureKind::from_str(arg_string) {
        println!("Printing help for {arg_string} procedure\n");
        println!("Description: {}", procedure.description());
        println!("Signarue: ({})", procedure.signature_string());
    }
}

fn main() {
    let context = Context::default();
    arbitrary_code(context.clone());
    repl(context);
}

fn arbitrary_code(context: Context) {
    //dbg!(parse("-x"));
    //dbg!(ast::ProcedureType::from_str("approx").unwrap().to_string());
}
