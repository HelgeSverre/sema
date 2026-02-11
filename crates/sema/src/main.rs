use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use sema_core::SemaError;
use sema_eval::Interpreter;

#[derive(Parser)]
#[command(name = "sema", about = "Sema: A Lisp with LLM primitives", version)]
struct Cli {
    /// File to execute
    file: Option<String>,

    /// Evaluate an expression and print result (if non-nil)
    #[arg(short, long, conflicts_with = "print")]
    eval: Option<String>,

    /// Evaluate an expression and always print result
    #[arg(short, long, conflicts_with = "eval")]
    print: Option<String>,

    /// Load file(s) before executing
    #[arg(short, long = "load", action = clap::ArgAction::Append)]
    load: Vec<String>,

    /// Suppress REPL banner
    #[arg(short, long)]
    quiet: bool,

    /// Enter REPL after running file or eval
    #[arg(short, long)]
    interactive: bool,

    /// Skip LLM auto-configuration
    #[arg(long)]
    no_init: bool,

    /// Disable LLM features (same as --no-init)
    #[arg(long, conflicts_with = "no_init")]
    no_llm: bool,

    /// Set default LLM model
    #[arg(long)]
    model: Option<String>,

    /// Set LLM provider (anthropic, openai)
    #[arg(long)]
    provider: Option<String>,

    /// Arguments passed to the script (after --)
    #[arg(last = true)]
    script_args: Vec<String>,
}

fn main() {
    let cli = Cli::parse();

    let interpreter = Interpreter::new();

    // Set LLM env vars before auto-configure
    if let Some(model) = &cli.model {
        std::env::set_var("SEMA_DEFAULT_MODEL", model);
    }
    if let Some(provider) = &cli.provider {
        std::env::set_var("SEMA_LLM_PROVIDER", provider);
    }

    // Auto-configure LLM unless --no-init or --no-llm
    if !cli.no_init && !cli.no_llm {
        let _ = interpreter.eval_str("(llm/auto-configure)");
    }

    // Load files first (in order)
    for load_file in &cli.load {
        let path = std::path::Path::new(load_file);
        if let Ok(canonical) = path.canonicalize() {
            sema_eval::push_file_path(canonical);
        }
        match std::fs::read_to_string(load_file) {
            Ok(content) => match interpreter.eval_str(&content) {
                Ok(_) => {
                    sema_eval::pop_file_path();
                }
                Err(e) => {
                    sema_eval::pop_file_path();
                    eprint!("Error loading {load_file}: ");
                    print_error(&e);
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("Error reading {load_file}: {e}");
                std::process::exit(1);
            }
        }
    }

    // Handle --eval
    if let Some(expr) = &cli.eval {
        match interpreter.eval_str(expr) {
            Ok(val) => {
                if !matches!(val, sema_core::Value::Nil) {
                    println!("{val}");
                }
            }
            Err(e) => {
                print_error(&e);
                std::process::exit(1);
            }
        }
        if cli.interactive {
            repl(interpreter, cli.quiet);
        }
        return;
    }

    // Handle --print
    if let Some(expr) = &cli.print {
        match interpreter.eval_str(expr) {
            Ok(val) => println!("{val}"),
            Err(e) => {
                print_error(&e);
                std::process::exit(1);
            }
        }
        if cli.interactive {
            repl(interpreter, cli.quiet);
        }
        return;
    }

    // Handle FILE
    if let Some(file) = &cli.file {
        let path = std::path::Path::new(file);
        if let Ok(canonical) = path.canonicalize() {
            sema_eval::push_file_path(canonical);
        }
        match std::fs::read_to_string(file) {
            Ok(content) => match interpreter.eval_str(&content) {
                Ok(_) => {
                    sema_eval::pop_file_path();
                }
                Err(e) => {
                    sema_eval::pop_file_path();
                    print_error(&e);
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("Error reading {file}: {e}");
                std::process::exit(1);
            }
        }
        if cli.interactive {
            repl(interpreter, cli.quiet);
        }
        return;
    }

    // REPL mode
    repl(interpreter, cli.quiet);
}

fn print_error(e: &SemaError) {
    eprintln!("Error: {}", e.inner());
    if let Some(trace) = e.stack_trace() {
        eprint!("{trace}");
    }
}

fn repl(interpreter: Interpreter, quiet: bool) {
    let mut rl = DefaultEditor::new().expect("failed to create editor");
    let history_path = dirs_path().join("history.txt");
    let _ = rl.load_history(&history_path);

    if !quiet {
        println!(
            "Sema v{} — A Lisp with LLM primitives",
            env!("CARGO_PKG_VERSION")
        );
        println!("Type ,help for help, ,quit to exit\n");
    }

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
                        print_error(&e);
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
    println!("  define/defun, lambda/fn, if, cond, let, let*, begin/do");
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
