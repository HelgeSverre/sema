use clap::{Parser, Subcommand};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use sema_core::{SemaError, Value};
use sema_eval::Interpreter;

#[derive(Parser)]
#[command(name = "sema", about = "Sema: A Lisp with LLM primitives", version)]
#[command(args_conflicts_with_subcommands = true)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

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

#[derive(Subcommand)]
enum Commands {
    /// Parse source and display the AST
    Ast {
        /// File to parse
        file: Option<String>,

        /// Expression to parse
        #[arg(short, long)]
        eval: Option<String>,

        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    // Handle subcommands
    if let Some(command) = cli.command {
        match command {
            Commands::Ast { file, eval, json } => {
                run_ast(file, eval, json);
            }
        }
        return;
    }

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
        if let Err(e) = interpreter.eval_str("(llm/auto-configure)") {
            if cli.provider.is_some() || cli.model.is_some() {
                print_error(&e);
                std::process::exit(1);
            }
        }
    }

    // Load files first (in order)
    for load_file in &cli.load {
        let path = std::path::Path::new(load_file);
        if let Ok(canonical) = path.canonicalize() {
            interpreter.ctx.push_file_path(canonical);
        }
        match std::fs::read_to_string(load_file) {
            Ok(content) => match interpreter.eval_str(&content) {
                Ok(_) => {
                    interpreter.ctx.pop_file_path();
                }
                Err(e) => {
                    interpreter.ctx.pop_file_path();
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
            interpreter.ctx.push_file_path(canonical);
        }
        match std::fs::read_to_string(file) {
            Ok(content) => match interpreter.eval_str(&content) {
                Ok(_) => {
                    interpreter.ctx.pop_file_path();
                }
                Err(e) => {
                    interpreter.ctx.pop_file_path();
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

fn run_ast(file: Option<String>, eval: Option<String>, json: bool) {
    let source = match (&file, &eval) {
        (Some(path), None) => match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Error reading {path}: {e}");
                std::process::exit(1);
            }
        },
        (None, Some(expr)) => expr.clone(),
        (Some(_), Some(_)) => {
            eprintln!("Error: cannot specify both a file and --eval");
            std::process::exit(1);
        }
        (None, None) => {
            eprintln!("Error: provide a file or --eval expression");
            std::process::exit(1);
        }
    };

    let exprs = match sema_reader::read_many(&source) {
        Ok(exprs) => exprs,
        Err(e) => {
            eprintln!("Parse error: {}", e.inner());
            std::process::exit(1);
        }
    };

    if json {
        let json_ast: Vec<serde_json::Value> = exprs.iter().map(value_to_ast_json).collect();
        let output = if json_ast.len() == 1 {
            serde_json::to_string_pretty(&json_ast[0]).unwrap()
        } else {
            serde_json::to_string_pretty(&json_ast).unwrap()
        };
        println!("{output}");
    } else {
        for (i, expr) in exprs.iter().enumerate() {
            if i > 0 {
                println!();
            }
            print_ast(expr, 0);
        }
    }
}

fn value_to_ast_json(val: &Value) -> serde_json::Value {
    match val {
        Value::Nil => serde_json::Value::Object(
            [("type".to_string(), serde_json::Value::String("nil".into()))]
                .into_iter()
                .collect(),
        ),
        Value::Bool(b) => serde_json::Value::Object(
            [
                ("type".to_string(), serde_json::Value::String("bool".into())),
                ("value".to_string(), serde_json::Value::Bool(*b)),
            ]
            .into_iter()
            .collect(),
        ),
        Value::Int(n) => serde_json::Value::Object(
            [
                ("type".to_string(), serde_json::Value::String("int".into())),
                ("value".to_string(), serde_json::Value::Number((*n).into())),
            ]
            .into_iter()
            .collect(),
        ),
        Value::Float(f) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("float".into()),
                ),
                (
                    "value".to_string(),
                    serde_json::Number::from_f64(*f)
                        .map(serde_json::Value::Number)
                        .unwrap_or(serde_json::Value::Null),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        Value::String(s) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("string".into()),
                ),
                (
                    "value".to_string(),
                    serde_json::Value::String(s.to_string()),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        Value::Symbol(s) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("symbol".into()),
                ),
                (
                    "value".to_string(),
                    serde_json::Value::String(sema_core::resolve(*s)),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        Value::Keyword(s) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("keyword".into()),
                ),
                (
                    "value".to_string(),
                    serde_json::Value::String(sema_core::resolve(*s)),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        Value::List(items) => serde_json::Value::Object(
            [
                ("type".to_string(), serde_json::Value::String("list".into())),
                (
                    "children".to_string(),
                    serde_json::Value::Array(items.iter().map(value_to_ast_json).collect()),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        Value::Vector(items) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("vector".into()),
                ),
                (
                    "children".to_string(),
                    serde_json::Value::Array(items.iter().map(value_to_ast_json).collect()),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        Value::Map(map) => serde_json::Value::Object(
            [
                ("type".to_string(), serde_json::Value::String("map".into())),
                (
                    "entries".to_string(),
                    serde_json::Value::Array(
                        map.iter()
                            .map(|(k, v)| {
                                serde_json::Value::Object(
                                    [
                                        ("key".to_string(), value_to_ast_json(k)),
                                        ("value".to_string(), value_to_ast_json(v)),
                                    ]
                                    .into_iter()
                                    .collect(),
                                )
                            })
                            .collect(),
                    ),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        other => serde_json::Value::Object(
            [(
                "type".to_string(),
                serde_json::Value::String(other.type_name().into()),
            )]
            .into_iter()
            .collect(),
        ),
    }
}

fn print_ast(val: &Value, indent: usize) {
    let pad = "  ".repeat(indent);
    match val {
        Value::Nil => println!("{pad}Nil"),
        Value::Bool(b) => println!("{pad}Bool {b}"),
        Value::Int(n) => println!("{pad}Int {n}"),
        Value::Float(f) => println!("{pad}Float {f}"),
        Value::String(s) => println!("{pad}String {s:?}"),
        Value::Symbol(s) => println!("{pad}Symbol {}", sema_core::resolve(*s)),
        Value::Keyword(s) => println!("{pad}Keyword :{}", sema_core::resolve(*s)),
        Value::List(items) => {
            println!("{pad}List");
            for item in items.iter() {
                print_ast(item, indent + 1);
            }
        }
        Value::Vector(items) => {
            println!("{pad}Vector");
            for item in items.iter() {
                print_ast(item, indent + 1);
            }
        }
        Value::Map(map) => {
            println!("{pad}Map");
            for (k, v) in map.iter() {
                println!("{pad}  Entry");
                print_ast(k, indent + 2);
                print_ast(v, indent + 2);
            }
        }
        other => println!("{pad}{}", other.type_name()),
    }
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
    user_bindings.sort_by_key(|(k, _)| *k);
    if user_bindings.is_empty() {
        println!("(no user-defined bindings)");
    } else {
        for (spur, val) in user_bindings {
            let name = sema_core::resolve(*spur);
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
