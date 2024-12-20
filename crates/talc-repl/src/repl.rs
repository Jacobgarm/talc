use itertools::Itertools;
use lazy_regex::{regex_captures, regex_find};
use std::borrow::Cow::{self, Borrowed, Owned};
use std::str::FromStr;
use strum::IntoEnumIterator;

use rustyline::completion::{Completer, Pair};
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::{CmdKind, Highlighter, MatchingBracketHighlighter};
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Completer, Helper, Hinter, Validator};
use rustyline::{CompletionType, Config, Editor};

use ast::ProcedureKind;
use context::Context;
use display::PrintOptions;
use parse::{is_valid_identifier, parse, preparse};
use talc::*;

#[derive(Helper, Completer, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Completer)]
    completer: ProcedureCompleter,
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    colored_prompt: String,
}

struct ProcedureCompleter {}

impl Completer for ProcedureCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        if pos == line.len()
            && let Some(partial) = regex_find!(r"[a-z]+$", line)
        {
            let start = pos - partial.len();
            let procs = ProcedureKind::iter().filter(|kind| kind.to_string().starts_with(partial));
            let pairs = procs
                .map(|kind| Pair {
                    display: kind.to_string(),
                    replacement: format!("{kind}["),
                })
                .collect_vec();
            Ok((start, pairs))
        } else {
            Ok((0, Vec::with_capacity(0)))
        }
    }
}

impl Highlighter for ReplHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize, kind: CmdKind) -> bool {
        self.highlighter.highlight_char(line, pos, kind)
    }
}

pub struct REPLContext {
    debug_mode: bool,
    pretty_printing: bool,
    decimal_rationals: Option<bool>,
}

impl Default for REPLContext {
    fn default() -> Self {
        Self {
            debug_mode: true,
            pretty_printing: true,
            decimal_rationals: None,
        }
    }
}

pub fn repl(mut eval_context: Context) {
    println!("\nStarting Talc REPL...");

    let prompt = "::> ";

    let config = Config::builder()
        .completion_type(CompletionType::List)
        .build();
    let helper = ReplHelper {
        completer: ProcedureCompleter {},
        highlighter: MatchingBracketHighlighter::new(),
        colored_prompt: format!("\x1b[1;32m{prompt}\x1b[0m"),
        validator: MatchingBracketValidator::new(),
    };

    let mut repl_context = REPLContext::default();

    let mut editor = Editor::with_config(config).unwrap();
    editor.set_helper(Some(helper));

    editor.set_auto_add_history(true);

    if editor.load_history("talc_history.txt").is_err() {
        println!("No history file found. Creating empty file \n");
    }

    'repl: loop {
        println!();
        let read_result = editor.readline(prompt);

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

        editor.save_history("talc_history.txt").unwrap();

        if let Some(remainder) = line.strip_prefix(":") {
            let shutdown = eval_command(remainder, &mut eval_context, &mut repl_context);
            if shutdown {
                println!("Exiting...");
                break;
            }
        } else if let Some((head, tail)) = line.split_once(":=") {
            let parse_result = parse(&preparse(tail.to_owned()));
            let parsed = match parse_result {
                Ok(exp) => exp,
                Err(error) => {
                    error.pretty_print(tail);
                    continue;
                }
            };

            let front = head.trim().to_string();

            if let Some((_, id, params_string)) = regex_captures!(r"^(.+)\((.*)\)$", &front) {
                if !is_valid_identifier(id) {
                    println!("Invalid function name: {id}");
                    continue;
                }
                let params = params_string
                    .split(",")
                    .map(ToOwned::to_owned)
                    .collect_vec();
                for param in &params {
                    if !is_valid_identifier(param) {
                        println!("Invalid parameter name: {param}");
                        continue 'repl;
                    }
                }
                eval_context.set_function(id.to_owned(), params, parsed);
                println!("Defined function {id}");
            } else {
                if !is_valid_identifier(&front) {
                    println!("Invalid variable name: {front}");
                    continue;
                }
                eval_context.set_var(front.to_owned(), parsed);
                println!("Defined variable {front}");
            }
        } else {
            let preparsed = parse::preparse(line.clone());
            if repl_context.debug_mode {
                println!("Preparsed: {preparsed}");
            }
            let parse_result = parse(&preparsed);
            let parsed = match parse_result {
                Ok(exp) => exp,
                Err(error) => {
                    error.pretty_print(&preparsed);
                    continue;
                }
            };

            let print_options = PrintOptions {
                decimal_rationals: repl_context
                    .decimal_rationals
                    .unwrap_or_else(|| line.contains('.')),
            };

            if repl_context.debug_mode {
                println!("AST: {parsed:?}");
                println!("Displays: {}", parsed.to_string_opts(print_options));
                println!();
            }

            let eval_result = parsed.eval(&eval_context);

            let evaluated = match eval_result {
                Ok(exp) => exp,
                Err(error) => {
                    dbg!(error);
                    continue;
                }
            };

            let s = if repl_context.pretty_printing {
                evaluated.to_pretty_string_opts(print_options)
            } else {
                evaluated.to_string_opts(print_options)
            };

            println!("{s}");

            if repl_context.debug_mode {
                println!();
                println!("Evaluated AST: {evaluated:?}");
            }
        }
    }
}

fn eval_command(command: &str, eval_context: &mut Context, repl_context: &mut REPLContext) -> bool {
    let Some(kind) = command.chars().next() else {
        return false;
    };
    let args = command[kind.len_utf8()..].trim().split(' ').collect_vec();
    match kind {
        'q' => return true,
        'h' => {
            for arg in args {
                print_help(arg, eval_context)
            }
        }
        's' => update_settings(args, repl_context),
        _ => println!("Unkown command type {kind}. Ignoring"),
    }

    false
}

fn update_settings(args: Vec<&str>, repl_context: &mut REPLContext) {
    match args[0] {
        "debug" => repl_context.debug_mode = true,
        "normal" => repl_context.debug_mode = false,
        "pretty" => repl_context.pretty_printing = true,
        "ugly" => repl_context.pretty_printing = false,
        _ => println!("Unknown setting"),
    }
}

fn print_help(arg_string: &str, eval_context: &Context) {
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
        println!("Signature: ({})", procedure.signature_string());
    }

    if let Some(func_info) = eval_context.get_function(arg_string) {
        println!("Printing help for {arg_string} function\n");
        println!("Signature: ({})", func_info.params.join(", "));
        if let Some(exp) = &func_info.exp {
            println!("Expression: {exp}");
        }
    }
}
