use std::rc::Rc;

use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::Shell;
use rustyline::completion::Completer;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use sema_core::{intern, pretty_print, Env, SemaError, Value, ValueView};
use sema_eval::{Interpreter, SPECIAL_FORM_NAMES};

mod archive;
mod import_tracer;

const REPL_COMMANDS: &[&str] = &[",quit", ",exit", ",q", ",help", ",h", ",env", ",builtins"];

struct SemaCompleter {
    env: Rc<Env>,
}

impl SemaCompleter {
    fn all_completions(&self, prefix: &str) -> Vec<String> {
        let mut candidates = Vec::new();

        // Collect all env bindings (walk parent chain)
        self.collect_env_bindings(&self.env, prefix, &mut candidates);

        // Special forms
        for &sf in SPECIAL_FORM_NAMES {
            if sf.starts_with(prefix) {
                candidates.push(sf.to_string());
            }
        }

        // REPL commands
        if prefix.starts_with(',') {
            for &cmd in REPL_COMMANDS {
                if cmd.starts_with(prefix) {
                    candidates.push(cmd.to_string());
                }
            }
        }

        candidates.sort();
        candidates.dedup();
        candidates
    }

    fn collect_env_bindings(&self, env: &Env, prefix: &str, candidates: &mut Vec<String>) {
        let bindings = env.bindings.borrow();
        for (spur, _) in bindings.iter() {
            let name = sema_core::resolve(*spur);
            if name.starts_with(prefix) {
                candidates.push(name);
            }
        }
        if let Some(parent) = &env.parent {
            self.collect_env_bindings(parent, prefix, candidates);
        }
    }
}

impl Completer for SemaCompleter {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<String>)> {
        let before = &line[..pos];
        let start = before
            .rfind(|c: char| c.is_whitespace() || c == '(' || c == '[' || c == '{' || c == '\'')
            .map(|i| i + 1)
            .unwrap_or(0);
        let prefix = &before[start..];
        if prefix.is_empty() {
            return Ok((start, vec![]));
        }
        Ok((start, self.all_completions(prefix)))
    }
}

impl rustyline::hint::Hinter for SemaCompleter {
    type Hint = String;
}
impl rustyline::highlight::Highlighter for SemaCompleter {}
impl rustyline::validate::Validator for SemaCompleter {}
impl rustyline::Helper for SemaCompleter {}

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

    /// Sandbox mode: restrict dangerous operations.
    /// Values: "strict", "all", or comma-separated list like "no-shell,no-network,no-fs-write"
    /// Available capabilities: shell, fs-read, fs-write, network, env-read, env-write, process, llm
    #[arg(long)]
    sandbox: Option<String>,

    /// Set default chat model
    #[arg(long)]
    chat_model: Option<String>,

    /// Set chat provider (anthropic, openai, gemini, groq, xai, mistral, moonshot, ollama)
    #[arg(long)]
    chat_provider: Option<String>,

    /// Set embedding model
    #[arg(long)]
    embedding_model: Option<String>,

    /// Set embedding provider (jina, voyage, cohere, openai)
    #[arg(long)]
    embedding_provider: Option<String>,

    /// Deprecated: use --chat-model instead
    #[arg(long, hide = true)]
    model: Option<String>,

    /// Deprecated: use --chat-provider instead
    #[arg(long, hide = true)]
    provider: Option<String>,

    /// Restrict file operations to these directories (comma-separated)
    #[arg(long, value_name = "DIRS")]
    allowed_paths: Option<String>,

    /// Use bytecode VM instead of tree-walker
    #[arg(long)]
    vm: bool,

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
    /// Generate shell completions
    Completions {
        /// Shell to generate completions for
        shell: Shell,
    },
    /// Compile source to bytecode (.semac)
    Compile {
        /// Source file to compile
        file: String,

        /// Output file path (default: input with .semac extension)
        #[arg(short, long)]
        output: Option<String>,

        /// Validate a .semac file without executing
        #[arg(long)]
        check: bool,
    },
    /// Disassemble a compiled bytecode file
    Disasm {
        /// Bytecode file to disassemble
        file: String,

        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Build a standalone executable from a sema source file
    Build {
        /// Source file to compile and bundle
        file: String,

        /// Output executable path (default: filename without extension)
        #[arg(short, long)]
        output: Option<String>,

        /// Additional files or directories to bundle (repeatable)
        #[arg(long = "include", action = clap::ArgAction::Append)]
        includes: Vec<String>,

        /// Sema binary to use as runtime base (default: current executable)
        #[arg(long)]
        runtime: Option<String>,
    },
    /// Format Sema source files
    Fmt {
        /// Files or glob patterns to format (default: **/*.sema in current directory)
        files: Vec<String>,

        /// Check formatting without writing changes (exit 1 if unformatted)
        #[arg(long)]
        check: bool,

        /// Print diff of formatting changes
        #[arg(long)]
        diff: bool,

        /// Max line width
        #[arg(long, default_value = "80")]
        width: usize,
    },
}

fn main() {
    // Check for embedded archive before parsing CLI args
    if let Some(exit_code) = try_run_embedded() {
        std::process::exit(exit_code);
    }

    let cli = Cli::parse();

    let sandbox = match &cli.sandbox {
        Some(value) => sema_core::Sandbox::parse_cli(value).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        }),
        None => sema_core::Sandbox::allow_all(),
    };
    let sandbox = match &cli.allowed_paths {
        Some(value) => {
            let paths = sema_core::Sandbox::parse_allowed_paths(value);
            sandbox.with_allowed_paths(paths)
        }
        None => sandbox,
    };

    // Handle subcommands
    if let Some(command) = cli.command {
        match command {
            Commands::Ast { file, eval, json } => {
                run_ast(file, eval, json);
            }
            Commands::Completions { shell } => {
                clap_complete::generate(shell, &mut Cli::command(), "sema", &mut std::io::stdout());
            }
            Commands::Compile {
                file,
                output,
                check,
            } => {
                if check {
                    run_check(&file);
                } else {
                    run_compile(&file, output.as_deref());
                }
            }
            Commands::Disasm { file, json } => {
                run_disasm(&file, json);
            }
            Commands::Build {
                file,
                output,
                includes,
                runtime,
            } => {
                run_build(&file, output.as_deref(), &includes, runtime.as_deref());
            }
            Commands::Fmt {
                files,
                check,
                diff,
                width,
            } => {
                run_fmt(&files, check, diff, width);
            }
        }
        return;
    }

    let interpreter = Interpreter::new_with_sandbox(&sandbox);

    // Set LLM env vars before auto-configure
    // New scoped flags take priority; fall back to deprecated --model/--provider
    let effective_chat_model = cli.chat_model.as_ref().or(cli.model.as_ref());
    let effective_chat_provider = cli.chat_provider.as_ref().or(cli.provider.as_ref());

    if cli.model.is_some() && cli.chat_model.is_none() {
        eprintln!("Warning: --model is deprecated, use --chat-model instead");
    }
    if cli.provider.is_some() && cli.chat_provider.is_none() {
        eprintln!("Warning: --provider is deprecated, use --chat-provider instead");
    }

    if let Some(model) = effective_chat_model {
        std::env::set_var("SEMA_CHAT_MODEL", model);
    }
    if let Some(provider) = effective_chat_provider {
        std::env::set_var("SEMA_CHAT_PROVIDER", provider);
    }
    if let Some(model) = &cli.embedding_model {
        std::env::set_var("SEMA_EMBEDDING_MODEL", model);
    }
    if let Some(provider) = &cli.embedding_provider {
        std::env::set_var("SEMA_EMBEDDING_PROVIDER", provider);
    }

    // Auto-configure LLM unless --no-init or --no-llm
    if !cli.no_init && !cli.no_llm {
        if let Err(e) = interpreter.eval_str("(llm/auto-configure)") {
            if effective_chat_provider.is_some() || effective_chat_model.is_some() {
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
            Ok(content) => match eval_with_mode(&interpreter, &content, cli.vm) {
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
        match eval_with_mode(&interpreter, expr, cli.vm) {
            Ok(val) => {
                if !val.is_nil() {
                    println!("{}", pretty_print(&val, 80));
                }
            }
            Err(e) => {
                print_error(&e);
                std::process::exit(1);
            }
        }
        if cli.interactive {
            repl(interpreter, cli.quiet, cli.sandbox.as_deref(), cli.vm);
        }
        return;
    }

    // Handle --print
    if let Some(expr) = &cli.print {
        match eval_with_mode(&interpreter, expr, cli.vm) {
            Ok(val) => println!("{val}"),
            Err(e) => {
                print_error(&e);
                std::process::exit(1);
            }
        }
        if cli.interactive {
            repl(interpreter, cli.quiet, cli.sandbox.as_deref(), cli.vm);
        }
        return;
    }

    // Handle FILE
    if let Some(file) = &cli.file {
        let path = std::path::Path::new(file);

        // Auto-detect .semac bytecode files
        if let Ok(bytes) = std::fs::read(path) {
            if sema_vm::is_bytecode_file(&bytes) {
                match run_bytecode_bytes(&interpreter, &bytes) {
                    Ok(_) => {}
                    Err(e) => {
                        print_error(&e);
                        std::process::exit(1);
                    }
                }
                if cli.interactive {
                    repl(interpreter, cli.quiet, cli.sandbox.as_deref(), cli.vm);
                }
                return;
            }
        }

        if let Ok(canonical) = path.canonicalize() {
            interpreter.ctx.push_file_path(canonical);
        }
        match std::fs::read_to_string(file) {
            Ok(content) => match eval_with_mode(&interpreter, &content, cli.vm) {
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
            repl(interpreter, cli.quiet, cli.sandbox.as_deref(), cli.vm);
        }
        return;
    }

    // REPL mode
    repl(interpreter, cli.quiet, cli.sandbox.as_deref(), cli.vm);
}

fn eval_with_mode(
    interpreter: &Interpreter,
    input: &str,
    use_vm: bool,
) -> Result<sema_core::Value, sema_core::SemaError> {
    if use_vm {
        interpreter.eval_str_compiled(input)
    } else {
        interpreter.eval_str(input)
    }
}

fn run_compile(file: &str, output: Option<&str>) {
    let path = std::path::Path::new(file);
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {file}: {e}");
            std::process::exit(1);
        }
    };

    // Compute source hash (CRC-32)
    let source_hash = crc32_simple(source.as_bytes());

    // Use Interpreter for macro expansion before compilation
    let sandbox = sema_core::Sandbox::allow_all();
    let interpreter = Interpreter::new_with_sandbox(&sandbox);

    let result = match interpreter.compile_to_bytecode(&source) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Compile error: {}", e.inner());
            std::process::exit(1);
        }
    };

    // Serialize
    let bytes = match sema_vm::serialize_to_bytes(&result, source_hash) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Serialization error: {}", e.inner());
            std::process::exit(1);
        }
    };

    // Write output
    let out_path = match output {
        Some(o) => std::path::PathBuf::from(o),
        None => path.with_extension("semac"),
    };
    if let Err(e) = std::fs::write(&out_path, &bytes) {
        eprintln!("Error writing {}: {e}", out_path.display());
        std::process::exit(1);
    }
}

fn try_run_embedded() -> Option<i32> {
    let exe_path = std::env::current_exe().ok()?;

    // Try named section first (macOS Mach-O / Windows PE via libsui),
    // fall back to trailer scan (Linux ELF raw append).
    let archive_data = if let Ok(Some(data)) = libsui::find_section("semaexec") {
        data.to_vec()
    } else if archive::has_embedded_archive(&exe_path).ok()? {
        match std::fs::read(&exe_path) {
            Ok(data) => {
                let len = data.len();
                let trailer = &data[len - 16..];
                let archive_size = u64::from_le_bytes(trailer[0..8].try_into().unwrap()) as usize;
                data[len - 16 - archive_size..len - 16].to_vec()
            }
            Err(_) => return None,
        }
    } else {
        return None;
    };

    let arch = match archive::deserialize_archive_from_bytes(&archive_data) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("Error: failed to load embedded archive: {e}");
            return Some(1);
        }
    };

    let entry_point = arch
        .metadata
        .get("entry-point")
        .and_then(|v| std::str::from_utf8(v).ok())
        .unwrap_or("__main__.semac")
        .to_string();

    let bytecode = match arch.files.get(&entry_point) {
        Some(b) => b.clone(),
        None => {
            eprintln!("Error: entry point '{entry_point}' not found in embedded archive");
            return Some(1);
        }
    };

    sema_core::vfs::init_vfs(arch.files);

    let sandbox = sema_core::Sandbox::allow_all();
    let interpreter = Interpreter::new_with_sandbox(&sandbox);

    let _ = interpreter.eval_str("(llm/auto-configure)");

    match run_bytecode_bytes(&interpreter, &bytecode) {
        Ok(_) => Some(0),
        Err(e) => {
            print_error(&e);
            Some(1)
        }
    }
}

fn run_build(file: &str, output: Option<&str>, includes: &[String], runtime: Option<&str>) {
    let path = std::path::Path::new(file);

    // Validate input file exists
    if !path.exists() {
        eprintln!("Error: source file not found: {file}");
        std::process::exit(1);
    }

    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {file}: {e}");
            std::process::exit(1);
        }
    };

    eprintln!("[1/5] Compiling {file}...");

    // Compute source hash and compile to bytecode
    let source_hash = crc32_simple(source.as_bytes());
    let sandbox = sema_core::Sandbox::allow_all();
    let interpreter = Interpreter::new_with_sandbox(&sandbox);

    let result = match interpreter.compile_to_bytecode(&source) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Compile error: {}", e.inner());
            std::process::exit(1);
        }
    };

    let bytecode = match sema_vm::serialize_to_bytes(&result, source_hash) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Serialization error: {}", e.inner());
            std::process::exit(1);
        }
    };

    eprintln!("[2/5] Tracing imports...");

    // Trace transitive imports
    let imports = match import_tracer::trace_imports(path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Error tracing imports: {e}");
            std::process::exit(1);
        }
    };

    eprintln!("[3/5] Collecting assets...");

    // Build VFS files map
    let mut files = std::collections::HashMap::new();

    // Entry point bytecode
    files.insert("__main__.semac".to_string(), bytecode);

    // Traced imports
    for (rel_path, contents) in &imports {
        if let Err(e) = sema_core::vfs::validate_vfs_path(rel_path) {
            eprintln!("Warning: skipping import with invalid VFS path: {e}");
            continue;
        }
        files.insert(rel_path.clone(), contents.clone());
    }

    // Additional --include assets
    for include in includes {
        let inc_path = std::path::Path::new(include);
        if inc_path.is_dir() {
            let base = inc_path
                .file_name()
                .unwrap_or(inc_path.as_os_str())
                .to_string_lossy()
                .to_string();
            collect_directory_files(inc_path, &base, &mut files);
        } else if inc_path.is_file() {
            let rel = inc_path
                .file_name()
                .unwrap_or(inc_path.as_os_str())
                .to_string_lossy()
                .to_string();
            if let Err(e) = sema_core::vfs::validate_vfs_path(&rel) {
                eprintln!("Warning: skipping {include}: {e}");
                continue;
            }
            match std::fs::read(inc_path) {
                Ok(data) => {
                    files.insert(rel, data);
                }
                Err(e) => {
                    eprintln!("Warning: cannot read {include}: {e}");
                }
            }
        } else {
            eprintln!("Warning: --include path not found: {include}");
        }
    }

    eprintln!("[4/5] Building archive ({} files)...", files.len());

    // Build metadata
    let mut metadata = std::collections::HashMap::new();
    metadata.insert(
        "sema-version".to_string(),
        env!("CARGO_PKG_VERSION").as_bytes().to_vec(),
    );
    metadata.insert(
        "build-timestamp".to_string(),
        build_timestamp().into_bytes(),
    );
    metadata.insert("entry-point".to_string(), b"__main__.semac".to_vec());

    let canonical_root = path
        .parent()
        .and_then(|p| p.canonicalize().ok())
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_default());
    metadata.insert(
        "build-root".to_string(),
        canonical_root.to_string_lossy().into_owned().into_bytes(),
    );

    let archive_bytes = archive::serialize_archive(&metadata, &files);

    eprintln!("[5/5] Writing executable...");

    // Determine output path
    let output_path = match output {
        Some(o) => std::path::PathBuf::from(o),
        None => {
            let stem = path.file_stem().unwrap_or(path.as_os_str());
            std::path::PathBuf::from(stem)
        }
    };

    // Determine runtime binary
    let runtime_path = match runtime {
        Some(r) => std::path::PathBuf::from(r),
        None => match std::env::current_exe() {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Error: cannot determine current executable path: {e}");
                std::process::exit(1);
            }
        },
    };

    if let Err(e) = write_executable_platform(&runtime_path, &output_path, &archive_bytes) {
        eprintln!("Error writing executable: {e}");
        std::process::exit(1);
    }

    eprintln!(
        "Built: {} ({} bytes, {} bundled files)",
        output_path.display(),
        std::fs::metadata(&output_path)
            .map(|m| m.len())
            .unwrap_or(0),
        files.len()
    );
}

/// Write the executable using platform-specific injection.
fn write_executable_platform(
    runtime_path: &std::path::Path,
    output_path: &std::path::Path,
    archive_bytes: &[u8],
) -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(target_os = "macos")]
    {
        let runtime = std::fs::read(runtime_path)?;
        let mut out = std::fs::File::create(output_path)?;
        libsui::Macho::from(runtime)?
            .write_section("semaexec", archive_bytes.to_vec())?
            .build_and_sign(&mut out)?;

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let perms = std::fs::Permissions::from_mode(0o755);
            std::fs::set_permissions(output_path, perms)?;
        }

        return Ok(());
    }

    #[cfg(target_os = "windows")]
    {
        let runtime = std::fs::read(runtime_path)?;
        let mut out = std::fs::File::create(output_path)?;
        libsui::PortableExecutable::from(&runtime)?
            .write_resource(&["semaexec"], archive_bytes.to_vec())?
            .build(&mut out)?;
        return Ok(());
    }

    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    {
        archive::write_bundled_executable(runtime_path, output_path, archive_bytes)?;
        return Ok(());
    }

    // Unreachable, but satisfies the compiler for all cfg combinations
    #[allow(unreachable_code)]
    Ok(())
}

/// Recursively collect files from a directory into the VFS files map.
fn collect_directory_files(
    dir: &std::path::Path,
    base: &str,
    files: &mut std::collections::HashMap<String, Vec<u8>>,
) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("Warning: cannot read directory {}: {e}", dir.display());
            return;
        }
    };

    for entry in entries.flatten() {
        let entry_path = entry.path();
        let name = entry.file_name().to_string_lossy().to_string();
        let vfs_path = if base.is_empty() {
            name.clone()
        } else {
            format!("{base}/{name}")
        };

        if entry_path.is_dir() {
            collect_directory_files(&entry_path, &vfs_path, files);
        } else if entry_path.is_file() {
            if let Err(e) = sema_core::vfs::validate_vfs_path(&vfs_path) {
                eprintln!("Warning: skipping {}: {e}", entry_path.display());
                continue;
            }
            match std::fs::read(&entry_path) {
                Ok(data) => {
                    files.insert(vfs_path, data);
                }
                Err(e) => {
                    eprintln!("Warning: cannot read {}: {e}", entry_path.display());
                }
            }
        }
    }
}

/// Return current Unix timestamp as a string (seconds since epoch).
fn build_timestamp() -> String {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs().to_string())
        .unwrap_or_else(|_| "0".to_string())
}

fn run_check(file: &str) {
    let bytes = match std::fs::read(file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("✗ {file}: {e}");
            std::process::exit(1);
        }
    };

    if !sema_vm::is_bytecode_file(&bytes) {
        eprintln!("✗ {file}: not a valid .semac bytecode file");
        std::process::exit(1);
    }

    // Read header info before full deserialization
    let format_version = u16::from_le_bytes([bytes[4], bytes[5]]);
    let major = u16::from_le_bytes([bytes[8], bytes[9]]);
    let minor = u16::from_le_bytes([bytes[10], bytes[11]]);
    let patch = u16::from_le_bytes([bytes[12], bytes[13]]);

    match sema_vm::deserialize_from_bytes(&bytes) {
        Ok(result) => {
            let n_funcs = result.functions.len();
            println!(
                "✓ {file}: valid (format v{format_version}, sema {major}.{minor}.{patch}, {n_funcs} function{}, {} bytes)",
                if n_funcs == 1 { "" } else { "s" },
                bytes.len()
            );
        }
        Err(e) => {
            eprintln!("✗ {file}: {}", e.inner());
            std::process::exit(1);
        }
    }
}

fn run_disasm(file: &str, json: bool) {
    let bytes = match std::fs::read(file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Error reading {file}: {e}");
            std::process::exit(1);
        }
    };

    if !sema_vm::is_bytecode_file(&bytes) {
        eprintln!("Error: {file} is not a valid .semac bytecode file");
        std::process::exit(1);
    }

    let result = match sema_vm::deserialize_from_bytes(&bytes) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Deserialization error: {}", e.inner());
            std::process::exit(1);
        }
    };

    if json {
        let json_val = disassemble_to_json(&result, &bytes);
        println!("{}", serde_json::to_string_pretty(&json_val).unwrap());
    } else {
        // Disassemble main chunk
        print!("{}", sema_vm::disassemble(&result.chunk, Some("<main>")));

        // Disassemble each function
        for (i, func) in result.functions.iter().enumerate() {
            let name = func
                .name
                .map(sema_core::resolve)
                .unwrap_or_else(|| format!("<fn {i}>"));
            print!("{}", sema_vm::disassemble(&func.chunk, Some(&name)));
        }
    }
}

fn disassemble_to_json(result: &sema_vm::CompileResult, bytes: &[u8]) -> serde_json::Value {
    let format_version = u16::from_le_bytes([bytes[4], bytes[5]]);
    let major = u16::from_le_bytes([bytes[8], bytes[9]]);
    let minor = u16::from_le_bytes([bytes[10], bytes[11]]);
    let patch = u16::from_le_bytes([bytes[12], bytes[13]]);

    let mut functions = Vec::new();

    // Main chunk
    functions.push(chunk_to_json(&result.chunk, "<main>"));

    // Function templates
    for (i, func) in result.functions.iter().enumerate() {
        let name = func
            .name
            .map(sema_core::resolve)
            .unwrap_or_else(|| format!("<fn {i}>"));
        let mut obj = chunk_to_json(&func.chunk, &name);
        obj["arity"] = serde_json::json!(func.arity);
        obj["has_rest"] = serde_json::json!(func.has_rest);
        obj["upvalues"] = serde_json::json!(func.upvalue_descs.len());
        functions.push(obj);
    }

    serde_json::json!({
        "format_version": format_version,
        "sema_version": format!("{major}.{minor}.{patch}"),
        "size_bytes": bytes.len(),
        "functions": functions,
    })
}

fn chunk_to_json(chunk: &sema_vm::Chunk, name: &str) -> serde_json::Value {
    let mut instructions = Vec::new();
    let code = &chunk.code;
    let mut pc = 0usize;

    while pc < code.len() {
        let op_byte = code[pc];
        let op = sema_vm::Op::from_u8(op_byte);
        let op_name = op
            .map(|o| format!("{o:?}"))
            .unwrap_or_else(|| format!("Unknown(0x{op_byte:02x})"));

        let (inst, next_pc) = match op {
            Some(sema_vm::Op::Const) => {
                let idx = u16::from_le_bytes([code[pc + 1], code[pc + 2]]);
                let val_str = chunk
                    .consts
                    .get(idx as usize)
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "?".into());
                (
                    serde_json::json!({"pc": pc, "op": op_name, "index": idx, "value": val_str}),
                    pc + 3,
                )
            }
            Some(
                sema_vm::Op::LoadLocal
                | sema_vm::Op::StoreLocal
                | sema_vm::Op::LoadUpvalue
                | sema_vm::Op::StoreUpvalue,
            ) => {
                let slot = u16::from_le_bytes([code[pc + 1], code[pc + 2]]);
                (
                    serde_json::json!({"pc": pc, "op": op_name, "slot": slot}),
                    pc + 3,
                )
            }
            Some(
                sema_vm::Op::LoadGlobal | sema_vm::Op::StoreGlobal | sema_vm::Op::DefineGlobal,
            ) => {
                let spur_bits =
                    u32::from_le_bytes([code[pc + 1], code[pc + 2], code[pc + 3], code[pc + 4]]);
                // Safety: the deserialized bytecode has already remapped indices to valid Spurs
                let name_str = if spur_bits != 0 {
                    let spur = unsafe { std::mem::transmute::<u32, sema_core::Spur>(spur_bits) };
                    sema_core::resolve(spur)
                } else {
                    format!("spur({spur_bits})")
                };
                (
                    serde_json::json!({"pc": pc, "op": op_name, "name": name_str}),
                    pc + 5,
                )
            }
            Some(sema_vm::Op::Jump | sema_vm::Op::JumpIfFalse | sema_vm::Op::JumpIfTrue) => {
                let offset =
                    i32::from_le_bytes([code[pc + 1], code[pc + 2], code[pc + 3], code[pc + 4]]);
                let target = (pc as i32 + 5 + offset) as u32;
                (
                    serde_json::json!({"pc": pc, "op": op_name, "offset": offset, "target": target}),
                    pc + 5,
                )
            }
            Some(sema_vm::Op::Call | sema_vm::Op::TailCall) => {
                let argc = u16::from_le_bytes([code[pc + 1], code[pc + 2]]);
                (
                    serde_json::json!({"pc": pc, "op": op_name, "argc": argc}),
                    pc + 3,
                )
            }
            Some(sema_vm::Op::CallNative) => {
                let native_id = u16::from_le_bytes([code[pc + 1], code[pc + 2]]);
                let argc = u16::from_le_bytes([code[pc + 3], code[pc + 4]]);
                (
                    serde_json::json!({"pc": pc, "op": op_name, "native_id": native_id, "argc": argc}),
                    pc + 5,
                )
            }
            Some(sema_vm::Op::MakeClosure) => {
                let func_id = u16::from_le_bytes([code[pc + 1], code[pc + 2]]);
                let n_upvalues = u16::from_le_bytes([code[pc + 3], code[pc + 4]]);
                let mut upvals = Vec::new();
                let mut upc = pc + 5;
                for _ in 0..n_upvalues {
                    let is_local = u16::from_le_bytes([code[upc], code[upc + 1]]);
                    let idx = u16::from_le_bytes([code[upc + 2], code[upc + 3]]);
                    upvals.push(serde_json::json!({"is_local": is_local != 0, "index": idx}));
                    upc += 4;
                }
                (
                    serde_json::json!({"pc": pc, "op": op_name, "func_id": func_id, "upvalues": upvals}),
                    upc,
                )
            }
            Some(
                sema_vm::Op::MakeList
                | sema_vm::Op::MakeVector
                | sema_vm::Op::MakeMap
                | sema_vm::Op::MakeHashMap,
            ) => {
                let count = u16::from_le_bytes([code[pc + 1], code[pc + 2]]);
                (
                    serde_json::json!({"pc": pc, "op": op_name, "count": count}),
                    pc + 3,
                )
            }
            _ => (serde_json::json!({"pc": pc, "op": op_name}), pc + 1),
        };

        instructions.push(inst);
        pc = next_pc;
    }

    let constants: Vec<String> = chunk.consts.iter().map(|v| v.to_string()).collect();

    serde_json::json!({
        "name": name,
        "n_locals": chunk.n_locals,
        "max_stack": chunk.max_stack,
        "code_bytes": chunk.code.len(),
        "constants": constants,
        "instructions": instructions,
        "exception_table": chunk.exception_table.iter().map(|e| {
            serde_json::json!({
                "try_start": e.try_start,
                "try_end": e.try_end,
                "handler_pc": e.handler_pc,
                "stack_depth": e.stack_depth,
                "catch_slot": e.catch_slot,
            })
        }).collect::<Vec<_>>(),
    })
}

fn run_bytecode_bytes(
    interpreter: &Interpreter,
    bytes: &[u8],
) -> Result<sema_core::Value, SemaError> {
    let result = sema_vm::deserialize_from_bytes(bytes)?;

    let functions: Vec<std::rc::Rc<sema_vm::Function>> =
        result.functions.into_iter().map(std::rc::Rc::new).collect();
    let closure = std::rc::Rc::new(sema_vm::Closure {
        func: std::rc::Rc::new(sema_vm::Function {
            name: None,
            chunk: result.chunk,
            upvalue_descs: Vec::new(),
            arity: 0,
            has_rest: false,
            local_names: Vec::new(),
        }),
        upvalues: Vec::new(),
    });

    let mut vm = sema_vm::VM::new(interpreter.global_env.clone(), functions);
    vm.execute(closure, &interpreter.ctx)
}

fn crc32_simple(data: &[u8]) -> u32 {
    let mut crc: u32 = 0xFFFF_FFFF;
    for &byte in data {
        crc ^= byte as u32;
        for _ in 0..8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ 0xEDB8_8320;
            } else {
                crc >>= 1;
            }
        }
    }
    !crc
}

fn run_fmt(patterns: &[String], check: bool, show_diff: bool, width: usize) {
    // Determine which files to format
    let files = if patterns.is_empty() {
        // Default: all .sema files in current directory recursively
        match glob::glob("**/*.sema") {
            Ok(paths) => paths
                .filter_map(|p| p.ok())
                .map(|p| p.to_string_lossy().to_string())
                .collect::<Vec<_>>(),
            Err(e) => {
                eprintln!("Error: invalid glob pattern: {e}");
                std::process::exit(1);
            }
        }
    } else {
        // Expand each pattern
        let mut all_files = Vec::new();
        for pattern in patterns {
            // If it contains glob characters, expand it
            if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
                match glob::glob(pattern) {
                    Ok(paths) => {
                        for path in paths.filter_map(|p| p.ok()) {
                            all_files.push(path.to_string_lossy().to_string());
                        }
                    }
                    Err(e) => {
                        eprintln!("Error: invalid glob pattern '{pattern}': {e}");
                        std::process::exit(1);
                    }
                }
            } else {
                // Treat as literal file path
                all_files.push(pattern.clone());
            }
        }
        all_files
    };

    if files.is_empty() {
        println!("No .sema files found");
        return;
    }

    let mut checked = 0;
    let mut changed = 0;
    let mut errors = 0;

    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {file}: {e}");
                errors += 1;
                continue;
            }
        };

        let formatted = match sema_fmt::format_source(&source, width) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Error formatting {file}: {e}");
                errors += 1;
                continue;
            }
        };

        checked += 1;

        if source != formatted {
            changed += 1;

            if check {
                println!("Would reformat: {file}");
            } else if show_diff {
                // Simple line-by-line diff
                print_simple_diff(file, &source, &formatted);
            } else {
                // Write formatted output back
                if let Err(e) = std::fs::write(file, &formatted) {
                    eprintln!("Error writing {file}: {e}");
                    errors += 1;
                    continue;
                }
                println!("Formatted: {file}");
            }
        }
    }

    // Print summary
    if check {
        if changed > 0 {
            println!("\n{changed} file(s) would be reformatted, {checked} file(s) checked");
            std::process::exit(1);
        } else {
            println!("{checked} file(s) already formatted");
        }
    } else if show_diff {
        println!("\n{changed} file(s) would change, {checked} file(s) checked");
    } else if changed > 0 {
        println!(
            "\n{changed} file(s) formatted, {} file(s) unchanged",
            checked - changed
        );
    } else {
        println!("{checked} file(s) already formatted");
    }

    if errors > 0 {
        eprintln!("{errors} error(s)");
        std::process::exit(1);
    }
}

fn print_simple_diff(filename: &str, old: &str, new: &str) {
    println!("--- {filename}");
    println!("+++ {filename}");
    let old_lines: Vec<&str> = old.lines().collect();
    let new_lines: Vec<&str> = new.lines().collect();

    // Simple context diff: show lines that differ
    let max_lines = old_lines.len().max(new_lines.len());
    let mut in_diff = false;
    let mut diff_start = 0;

    for i in 0..max_lines {
        let old_line = old_lines.get(i).copied().unwrap_or("");
        let new_line = new_lines.get(i).copied().unwrap_or("");

        if old_line != new_line {
            if !in_diff {
                diff_start = i;
                in_diff = true;
                println!("@@ -{} +{} @@", i + 1, i + 1);
            }
            if i < old_lines.len() {
                println!("-{old_line}");
            }
            if i < new_lines.len() {
                println!("+{new_line}");
            }
        } else if in_diff && i - diff_start < 3 {
            println!(" {old_line}");
        } else {
            in_diff = false;
        }
    }
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
    match val.view() {
        ValueView::Nil => serde_json::Value::Object(
            [("type".to_string(), serde_json::Value::String("nil".into()))]
                .into_iter()
                .collect(),
        ),
        ValueView::Bool(b) => serde_json::Value::Object(
            [
                ("type".to_string(), serde_json::Value::String("bool".into())),
                ("value".to_string(), serde_json::Value::Bool(b)),
            ]
            .into_iter()
            .collect(),
        ),
        ValueView::Int(n) => serde_json::Value::Object(
            [
                ("type".to_string(), serde_json::Value::String("int".into())),
                ("value".to_string(), serde_json::Value::Number(n.into())),
            ]
            .into_iter()
            .collect(),
        ),
        ValueView::Float(f) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("float".into()),
                ),
                (
                    "value".to_string(),
                    serde_json::Number::from_f64(f)
                        .map(serde_json::Value::Number)
                        .unwrap_or(serde_json::Value::Null),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        ValueView::String(s) => serde_json::Value::Object(
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
        ValueView::Symbol(s) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("symbol".into()),
                ),
                (
                    "value".to_string(),
                    serde_json::Value::String(sema_core::resolve(s)),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        ValueView::Keyword(s) => serde_json::Value::Object(
            [
                (
                    "type".to_string(),
                    serde_json::Value::String("keyword".into()),
                ),
                (
                    "value".to_string(),
                    serde_json::Value::String(sema_core::resolve(s)),
                ),
            ]
            .into_iter()
            .collect(),
        ),
        ValueView::List(items) => serde_json::Value::Object(
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
        ValueView::Vector(items) => serde_json::Value::Object(
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
        ValueView::Map(map) => serde_json::Value::Object(
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
        _ => serde_json::Value::Object(
            [(
                "type".to_string(),
                serde_json::Value::String(val.type_name().into()),
            )]
            .into_iter()
            .collect(),
        ),
    }
}

fn print_ast(val: &Value, indent: usize) {
    let pad = "  ".repeat(indent);
    match val.view() {
        ValueView::Nil => println!("{pad}Nil"),
        ValueView::Bool(b) => println!("{pad}Bool {b}"),
        ValueView::Int(n) => println!("{pad}Int {n}"),
        ValueView::Float(f) => println!("{pad}Float {f}"),
        ValueView::String(s) => println!("{pad}String {s:?}"),
        ValueView::Symbol(s) => println!("{pad}Symbol {}", sema_core::resolve(s)),
        ValueView::Keyword(s) => println!("{pad}Keyword :{}", sema_core::resolve(s)),
        ValueView::List(items) => {
            println!("{pad}List");
            for item in items.iter() {
                print_ast(item, indent + 1);
            }
        }
        ValueView::Vector(items) => {
            println!("{pad}Vector");
            for item in items.iter() {
                print_ast(item, indent + 1);
            }
        }
        ValueView::Map(map) => {
            println!("{pad}Map");
            for (k, v) in map.iter() {
                println!("{pad}  Entry");
                print_ast(k, indent + 2);
                print_ast(v, indent + 2);
            }
        }
        _ => println!("{pad}{}", val.type_name()),
    }
}

fn print_error(e: &SemaError) {
    eprintln!("Error: {}", e.inner());
    if let Some(trace) = e.stack_trace() {
        eprint!("{trace}");
    }
    if let Some(hint) = e.hint() {
        eprintln!("  hint: {hint}");
    }
    if let Some(note) = e.note() {
        eprintln!("  note: {note}");
    }
}

fn repl(interpreter: Interpreter, quiet: bool, sandbox_mode: Option<&str>, use_vm: bool) {
    let env = interpreter.global_env.clone();
    env.set(intern("*1"), Value::nil());
    env.set(intern("*2"), Value::nil());
    env.set(intern("*3"), Value::nil());
    env.set(intern("*e"), Value::nil());
    let mut rl = Editor::new().expect("failed to create editor");
    rl.set_helper(Some(SemaCompleter { env: env.clone() }));
    let history_path = dirs_path().join("history.txt");
    let _ = rl.load_history(&history_path);

    if !quiet {
        println!(
            "Sema v{} — A Lisp with LLM primitives",
            env!("CARGO_PKG_VERSION")
        );
        if let Some(mode) = sandbox_mode {
            println!("Sandbox: {mode}");
        }
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
                        ",builtins" => {
                            print_builtins(&interpreter);
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

                match eval_with_mode(&interpreter, &input, use_vm) {
                    Ok(val) => {
                        if let Some(v1) = env.get(intern("*1")) {
                            if let Some(v2) = env.get(intern("*2")) {
                                env.set(intern("*3"), v2);
                            }
                            env.set(intern("*2"), v1);
                        }
                        env.set(intern("*1"), val.clone());
                        if !val.is_nil() {
                            println!("{}", pretty_print(&val, 80));
                        }
                    }
                    Err(e) => {
                        env.set(intern("*e"), Value::string(&e.to_string()));
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
    println!("  ,builtins     List all builtin functions");
    println!();
    println!("LLM Quick Start:");
    println!("  Set ANTHROPIC_API_KEY or OPENAI_API_KEY env var, then:");
    println!("  (llm/complete \"Hello!\")");
    println!("  (llm/chat [(message :user \"Hi\")] {{:model \"claude-haiku-4-5-20251001\"}})");
    println!();
    println!("History Variables:");
    println!("  *1, *2, *3   Last three results (most recent first)");
    println!("  *e           Last error message");
    println!();
    println!("Core Forms:");
    println!("  define/defun, lambda/fn, if, cond, let, let*, begin/do");
    println!("  quote, quasiquote, defmacro, and, or, when, unless");
}

fn print_env(interpreter: &Interpreter) {
    let bindings = interpreter.global_env.bindings.borrow();
    let mut user_bindings: Vec<_> = bindings
        .iter()
        .filter(|(_, v)| v.as_native_fn_rc().is_none())
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

fn print_builtins(interpreter: &Interpreter) {
    let bindings = interpreter.global_env.bindings.borrow();
    let mut names: Vec<_> = bindings
        .iter()
        .filter(|(_, v)| v.as_native_fn_rc().is_some())
        .map(|(spur, _)| sema_core::resolve(*spur))
        .collect();
    names.sort();

    if names.is_empty() {
        println!("(no builtin functions)");
        return;
    }

    let max_width = names.iter().map(|n| n.len()).max().unwrap_or(0) + 2;
    let term_width = 80;
    let cols = (term_width / max_width).max(1);

    for chunk in names.chunks(cols) {
        for name in chunk {
            print!("{name:<max_width$}");
        }
        println!();
    }
    println!("\n{} builtin functions", names.len());
}

fn dirs_path() -> std::path::PathBuf {
    sema_core::sema_home()
}
