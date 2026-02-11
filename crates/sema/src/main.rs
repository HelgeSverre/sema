use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use sema_eval::Interpreter;

#[derive(Parser)]
#[command(name = "sema", about = "Sema: A Lisp with LLM primitives")]
struct Cli {
    /// File to execute
    file: Option<String>,

    /// Evaluate an expression
    #[arg(short, long)]
    eval: Option<String>,
}

fn main() {
    let cli = Cli::parse();

    let interpreter = Interpreter::new();

    // Try auto-configure LLM from env vars
    let _ = interpreter.eval_str("(llm/auto-configure)");

    if let Some(expr) = &cli.eval {
        match interpreter.eval_str(expr) {
            Ok(val) => {
                if !matches!(val, sema_core::Value::Nil) {
                    println!("{val}");
                }
            }
            Err(e) => {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
        return;
    }

    if let Some(file) = &cli.file {
        match std::fs::read_to_string(file) {
            Ok(content) => match interpreter.eval_str(&content) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Error in {file}: {e}");
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("Error reading {file}: {e}");
                std::process::exit(1);
            }
        }
        return;
    }

    // REPL mode
    repl(interpreter);
}

fn repl(interpreter: Interpreter) {
    let mut rl = DefaultEditor::new().expect("failed to create editor");
    let history_path = dirs_path().join("history.txt");
    let _ = rl.load_history(&history_path);

    println!("Sema v0.1.0 — A Lisp with LLM primitives");
    println!("Type ,help for help, ,quit to exit\n");

    let mut buffer = String::new();
    let mut in_multiline = false;

    loop {
        let prompt = if in_multiline { "  ... " } else { "sema> " };
        match rl.readline(prompt) {
            Ok(line) => {
                let trimmed = line.trim();

                // Handle REPL commands
                if !in_multiline {
                    match trimmed {
                        ",quit" | ",exit" | ",q" => break,
                        ",help" | ",h" => {
                            print_help();
                            continue;
                        }
                        ",env" => {
                            print_env(&interpreter);
                            continue;
                        }
                        _ => {}
                    }
                }

                if in_multiline {
                    buffer.push('\n');
                    buffer.push_str(&line);
                } else {
                    buffer = line.clone();
                }

                // Check if parens are balanced
                if !is_balanced(&buffer) {
                    in_multiline = true;
                    continue;
                }

                in_multiline = false;
                let input = buffer.trim().to_string();
                buffer.clear();

                if input.is_empty() {
                    continue;
                }

                let _ = rl.add_history_entry(&input);

                match interpreter.eval_str(&input) {
                    Ok(val) => {
                        if !matches!(val, sema_core::Value::Nil) {
                            println!("{val}");
                        }
                    }
                    Err(e) => {
                        eprintln!("Error: {e}");
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                if in_multiline {
                    buffer.clear();
                    in_multiline = false;
                    println!("^C");
                    continue;
                }
                break;
            }
            Err(ReadlineError::Eof) => break,
            Err(e) => {
                eprintln!("Error: {e}");
                break;
            }
        }
    }

    let _ = std::fs::create_dir_all(dirs_path());
    let _ = rl.save_history(&history_path);
    println!("Goodbye!");
}

fn is_balanced(input: &str) -> bool {
    let mut depth = 0i32;
    let mut in_string = false;
    let mut escape = false;
    for ch in input.chars() {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' && in_string {
            escape = true;
            continue;
        }
        if ch == '"' {
            in_string = !in_string;
            continue;
        }
        if in_string {
            continue;
        }
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            _ => {}
        }
    }
    depth <= 0 && !in_string
}

fn print_help() {
    println!("Sema REPL Commands:");
    println!("  ,quit / ,q    Exit the REPL");
    println!("  ,help / ,h    Show this help");
    println!("  ,env          Show defined variables");
    println!();
    println!("LLM Quick Start:");
    println!("  Set ANTHROPIC_API_KEY or OPENAI_API_KEY env var, then:");
    println!("  (llm/complete \"Hello!\")");
    println!("  (llm/chat [(message :user \"Hi\")] {{:model \"claude-haiku-4-5-20251001\"}})");
    println!();
    println!("Core Forms:");
    println!("  define, lambda/fn, if, cond, let, let*, begin/do");
    println!("  quote, quasiquote, defmacro, and, or, when, unless");
}

fn print_env(interpreter: &Interpreter) {
    let bindings = interpreter.global_env.bindings.borrow();
    let mut user_bindings: Vec<_> = bindings
        .iter()
        .filter(|(_, v)| !matches!(v, sema_core::Value::NativeFn(_)))
        .collect();
    user_bindings.sort_by_key(|(k, _)| (*k).clone());
    if user_bindings.is_empty() {
        println!("(no user-defined bindings)");
    } else {
        for (name, val) in user_bindings {
            println!("  {name} = {val}");
        }
    }
}

fn dirs_path() -> std::path::PathBuf {
    dirs_home().join(".sema")
}

fn dirs_home() -> std::path::PathBuf {
    std::env::var("HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| std::path::PathBuf::from("."))
}
