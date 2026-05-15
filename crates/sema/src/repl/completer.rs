use std::cell::RefCell;
use std::rc::Rc;

use reedline::{Completer, Span, Suggestion};
use sema_core::Env;
use sema_eval::SPECIAL_FORM_NAMES;

use super::commands::REPL_COMMANDS;

// `reedline::Completer: Send`, but Sema's `Env` is built on `Rc<RefCell<_>>`
// and is fundamentally !Send. The REPL only ever runs on a single thread
// (the binary is single-threaded), so we stash the env in a thread-local
// and give the completer a zero-sized struct that is trivially Send.
thread_local! {
    static COMPLETER_ENV: RefCell<Option<Rc<Env>>> = const { RefCell::new(None) };
}

pub fn set_completer_env(env: Rc<Env>) {
    COMPLETER_ENV.with(|cell| *cell.borrow_mut() = Some(env));
}

/// Completes Sema symbols, special forms, and REPL meta-commands.
pub struct SemaCompleter;

impl SemaCompleter {
    pub fn new() -> Self {
        Self
    }

    fn collect(&self, prefix: &str) -> Vec<String> {
        let mut out = Vec::new();

        COMPLETER_ENV.with(|cell| {
            let borrow = cell.borrow();
            if let Some(env) = borrow.as_ref() {
                collect_env(env, prefix, &mut out);
            }
        });

        for &sf in SPECIAL_FORM_NAMES {
            if sf.starts_with(prefix) {
                out.push(sf.to_string());
            }
        }

        if prefix.starts_with(',') {
            for &cmd in REPL_COMMANDS {
                if cmd.starts_with(prefix) {
                    out.push(cmd.to_string());
                }
            }
        }

        out.sort();
        out.dedup();
        out
    }
}

fn collect_env(env: &Env, prefix: &str, out: &mut Vec<String>) {
    env.iter_bindings(|spur, _| {
        let name = sema_core::resolve(spur);
        if name.starts_with(prefix) {
            out.push(name);
        }
    });
    if let Some(parent) = &env.parent {
        collect_env(parent, prefix, out);
    }
}

impl Completer for SemaCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let before = &line[..pos];
        // Mirror the old rustyline behaviour: a "word" is anything since the
        // last whitespace or opening bracket / quote.
        let start = before
            .rfind(|c: char| c.is_whitespace() || c == '(' || c == '[' || c == '{' || c == '\'')
            .map(|i| i + 1)
            .unwrap_or(0);
        let prefix = &before[start..];
        if prefix.is_empty() {
            return vec![];
        }

        let names = self.collect(prefix);
        names
            .into_iter()
            .map(|name| Suggestion {
                value: name,
                description: None,
                style: None,
                extra: None,
                span: Span::new(start, pos),
                append_whitespace: false,
                display_override: None,
                match_indices: None,
            })
            .collect()
    }
}
