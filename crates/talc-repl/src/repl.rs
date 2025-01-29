use itertools::Itertools;
use lazy_regex::regex_find;
use std::borrow::Cow::{self, Borrowed, Owned};
use strum::IntoEnumIterator;

use rustyline::completion::{Completer, Pair};
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::highlight::{CmdKind, Highlighter, MatchingBracketHighlighter};
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Completer, Helper, Hinter, Validator};
use rustyline::{CompletionType, Config, Editor};

use talc::ast::ProcedureKind;
use talc::context::Context;

use crate::exec::{ExecContext, ExecResult, exec_line};

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

pub fn repl(math_ctx: &mut Context) {
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

    let mut editor = Editor::with_config(config).unwrap();
    editor.set_helper(Some(helper));

    editor.set_auto_add_history(true);

    if editor.load_history("talc_history.txt").is_err() {
        println!("No history file found. Creating empty file \n");
    }

    let mut exec_ctx = ExecContext {
        is_repl: true,
        ..Default::default()
    };

    loop {
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

        let res = exec_line(&line, &mut exec_ctx, math_ctx);

        match res {
            Some(ExecResult::Exit) => {
                println!("Exiting...");
                break;
            }
            Some(ExecResult::Error) => (),
            None => (),
        }
    }
}
