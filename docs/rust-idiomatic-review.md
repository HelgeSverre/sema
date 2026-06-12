# Sema Lisp — Style, Security, and Architectural Review

This document presents a deep-dive technical assessment of the **Sema Lisp** monorepo codebase. It evaluates the project's architecture, memory model, compiler/VM safety, language server protocol (LSP) implementation, and sandbox security constraints against the state-of-the-art in Rust system engineering.

Recommendations and findings are backed by citations of standard Rust guidelines (e.g., [mre/idiomatic-rust](https://github.com/mre/idiomatic-rust), the [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)) and security auditing practices.

---

## 1. Monorepo Architecture & Dependency Hygiene

Sema uses a multi-crate Cargo workspace divided into 13 distinct crates, implementing a clear separation of concerns (dependency flow: `sema-core ← sema-reader ← sema-vm ← sema-eval ← sema-stdlib/sema-llm ← sema`).

### 1.1 Dependency Duplication and Version Skew
Sema utilizes `[workspace.dependencies]` in the root [Cargo.toml](file:///Users/helge/code/sema-lisp/Cargo.toml) to enforce version lock-step. However, two frontend/target crates bypass this centralization:
1. **[crates/sema-wasm/Cargo.toml](file:///Users/helge/code/sema-lisp/crates/sema-wasm/Cargo.toml)**:
   * Explicitly redeclares path dependencies (e.g., `sema-core = { path = "../sema-core" }`) rather than inheriting from the workspace: `sema-core.workspace = true`.
   * Directly declares third-party libraries (e.g., `wasm-bindgen`, `js-sys`, `web-sys`, `wasm-bindgen-futures`, `getrandom`) with hardcoded versions instead of lifting them to the root.
2. **[crates/sema-notebook/Cargo.toml](file:///Users/helge/code/sema-lisp/crates/sema-notebook/Cargo.toml)**:
   * Declares `gag = "1.0.0"` locally, bypassing the workspace dependencies block.

### 1.2 Clippy Suppression Antipattern
Almost all crate entry points (e.g. `lib.rs` in `sema-core`, `sema-eval`, `sema-stdlib`, `sema-vm`) declare `#![allow(clippy::mutable_key_type)]` and `#![allow(clippy::cloned_ref_to_slice_refs)]`.
* `clippy::mutable_key_type` triggers because `Value` uses interior mutability (due to Nan-boxing wrapping pointer allocations like `Rc<RefCell<...>>`) and is used as keys in `HashMap` or `HashSet` (e.g., inside environment frames).
* *Architectural Risk*: Placing this allowance at the crate root suppresses legitimate bugs where key objects mutate *after* insertion, leading to orphan hash values and memory leaks or logically unreachable map nodes.

---

## 2. Virtual Machine Deep-Dive: Safety & Robustness

The virtual machine (`sema-vm`) is a stack-based bytecode execution engine. It uses NaN-boxing for the representation of Lisp values (wrapping pointers and immediates into a single `u64`).

### 2.1 Stack Underflow Vulnerability (`pop_unchecked`)
In the core dispatch loop of [crates/sema-vm/src/vm.rs:555](file:///Users/helge/code/sema-lisp/crates/sema-vm/src/vm.rs#L555), the helper function `pop_unchecked` is implemented as:
```rust
unsafe fn pop_unchecked(stack: &mut Vec<Value>) -> Value {
    let len = stack.len();
    debug_assert!(len > 0, "pop_unchecked on empty stack");
    let v = std::ptr::read(stack.as_ptr().add(len - 1));
    stack.set_len(len - 1);
    v
}
```
* *The Bug*: This function assumes that the compiler has guaranteed stack balance. While this is true for bytecode produced in-memory by the compiler, **it is not validated for deserialized `.semac` files** loaded from disk or network.
* *The Risk*: A handcrafted `.semac` file with an unbalanced instruction sequence (e.g. executing `Pop` or `Dup` on an empty stack) causes `len - 1` to wrap to `usize::MAX`. In release builds (where `debug_assert!` is compiled out), this causes `std::ptr::read` to perform out-of-bounds reads and writes `stack.set_len(usize::MAX)`, corrupting arbitrary memory.

### 2.2 Bytecode Verification Deficit
The bytecode deserializer in [serialize.rs](file:///Users/helge/code/sema-lisp/crates/sema-vm/src/serialize.rs) implements `validate_bytecode`, which checks:
1. Magic header and version numbers.
2. Jump targets (verifying that they land on valid instruction boundaries and not in the middle of operands).
3. Constant pool and upvalue descriptor bounds.

However, it **lacks a stack depth/balance validator** (abstract interpreter). A complete bytecode validator must simulate execution paths to compute the exact stack depth at each program point, rejecting chunks that underflow the stack or exceed the stack frame limits.

### 2.3 Closure Upvalue Leak in HOF Callbacks (Upvalue Closing)
SemaVM implements Lua-style open upvalues ([vm.rs:25](file:///Users/helge/code/sema-lisp/crates/sema-vm/src/vm.rs#L25)). When a closure captures local variables, they refer to stack offsets. When the declaring stack frame exits, they are migrated to the heap ("closed").
* *The Bug*: To support higher-order functions (HOFs) written in Rust (like `map`, `filter`, `retry`), the VM converts Lisp closures into `NativeFn` wrappers. When these HOFs invoke the closure, the VM executes them on a *fresh* VM context (`NativeFn::func`).
* *Divergence*: To make this safe, the VM calls `close_open_upvalues` *before* entering the native HOF bridge. This acts as a snapshot. If the closure mutates its captured variable via `set!`, the mutation lands in the closed snapshot on the heap, and **the local variable on the parent stack remains unchanged**.
* *Reproduction*:
  ```scheme
  (let ((c 0)) 
    (map (fn (x) (set! c (+ c x))) (list 1 2 3)) 
    c)
  ```
  Returns `6` on the Tree-walker, but `0` on the Bytecode VM.

---

## 3. Sandboxing & Security Gaps (SSRF Vulnerability)

Sema provides sandboxed execution limits via `Caps::FS_WRITE`, `Caps::NET`, etc. These restrictions are enforced when registering native builtins.

### 3.1 DNS-Rebinding / SSRF Bypass
In [crates/sema-llm/src/builtins.rs:711](file:///Users/helge/code/sema-lisp/crates/sema-llm/src/builtins.rs#L711), the function `guard_provider_url` is used to prevent sandboxed code from reaching local or loopback targets (Server-Side Request Forgery):
```rust
fn guard_provider_url(unrestricted: bool, opts: &BTreeMap<Value, Value>) -> Result<(), SemaError> {
    if unrestricted { return Ok(()); }
    let url = get_opt_string(opts, "base-url").or_else(|| get_opt_string(opts, "host"));
    if let Some(url) = url {
        if url_host(&url).is_some_and(|h| is_internal_host(&h)) {
            return Err(SemaError::eval(...));
        }
    }
    Ok(())
}
```
* *The Bug*: The validation of `is_internal_host` only parses the host string *prior* to connection. If a hostname is supplied (e.g. `evil-endpoint.local-check.com`), the host parsing checks fail to match loopback or private subnets. The URL is approved, and `reqwest` later resolves the domain to `127.0.0.1` or `10.0.0.1` at connection time.
* *The Risk*: Sandboxed users can pivot to the host's loopback interface, accessing metadata endpoints, databases, or local LLM instances (like Ollama).

### 3.2 Custom URL Parsing Differential
The helper function `url_host` in `builtins.rs:615` parses URLs manually to extract the hostname:
```rust
fn url_host(url: &str) -> Option<String> {
    let after = url.split_once("://").map(|(_, rest)| rest).unwrap_or(url);
    let authority = after.split(['/', '?', '#']).next().unwrap_or("");
    ...
}
```
* *The Bug*: Writing a custom URL parser is a well-known security antipattern. Discrepancies between custom string-slicing logic and the actual parser in `reqwest`/`hyper` (e.g., handling backslashes, userinfo separators `@`, IPv6 square brackets, or encoded characters) can be exploited to bypass checks (a Parser Differential attack).

---

## 4. Tree-Walker vs. VM Divergences

Sema maintains two execution engines. While dual-eval tests exist, there are structural divergences:

### 4.1 Missing VM Stack Traces
The tree-walker backend captures detailed call frames and spans, injecting a `:stack-trace` element into error maps caught by `try-catch`.
* On the Bytecode VM, stack traces are **omitted from caught error maps**, and runtime crashes print limited location tags. This forces developers to use `--tw` (tree-walker) to debug issues, defeating the purpose of the VM as the primary execution engine.

### 4.2 Eval Environment Scoping
In the tree-walker, `(eval expr)` evaluates code within the caller's lexical scope.
* In the VM, `eval` is delegated to `__vm-eval`, which accesses the **global environment only**. Lexical locals are invisible.
  ```scheme
  (define (run x) (eval 'x)) 
  (run 42) ; => Unbound variable: x (on VM)
  ```

### 4.3 Type Reflection Discrepancy
Reflecting on closure types diverges between backends:
```scheme
(type (fn (x) x))
```
* Tree-Walker returns `:lambda`.
* VM returns `:native-fn` (due to the `NativeFn` wrapper applied for stdlib HOF interop).

---

## 5. LSP Architecture & Monolithic Implementations

The Language Server Protocol (`sema-lsp`) is a single-threaded server. It employs an actor pattern to run queries against a shared state.

### 5.1 The 4,800+ Line Monolith
The file [crates/sema-lsp/src/lib.rs](file:///Users/helge/code/sema-lisp/crates/sema-lsp/src/lib.rs) is **~180 KB and contains 4,826 lines of code**. It handles AST re-parsing, diagnostics compilation, command execution, and LSP endpoints (completion, formatting, semantic tokens, hover, definition, and signature help).
* *Architectural Smell*: This violates the Single Responsibility Principle. Legitimate modifications to hover logic require navigating the same file that sets up TCP sockets and parses input frames.

### 5.2 Single-Threaded Actor Design (Decision Justification)
The LSP uses a single-threaded execution thread:
```rust
let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<LspRequest>();
let backend_handle = std::thread::spawn(move || {
    let mut state = BackendState::new();
    while let Some(req) = rx.blocking_recv() { ... }
});
```
* *Why it is idiomatic*: The core `Value` type and `Env` environment of Sema use `Rc` and `RefCell` (non-Send, single-threaded pointers). If the LSP ran requests concurrently across threads, it would require wrapping the entire interpreter state in `Arc<Mutex<Env>>` or `RwLock`, significantly slowing down evaluation and introducing deadlocks. The single-threaded loop treats `BackendState` as an actor, keeping the non-Send state safely pinned to a single thread.

### 5.3 Cooperative Workspace Scanning
To prevent workspace-wide scanning from blocking interactive requests, the scanner is implemented cooperatively using deferred queues:
```rust
LspRequest::ScanWorkspace { root } => {
    let scanner = WorkspaceScanner::new(&root);
    deferred.push_back(LspRequest::ScanWorkspaceContinue { scanner });
}
```
* *How it works*: `ScanWorkspaceContinue` processes a batch of 10 files and then re-enqueues itself at the back of the queue. This ensures that any typing/completion requests sent by the client are processed immediately, preventing LSP freezes during initial imports.

---

## 6. Actionable Implementation Plan

Below is the prioritized roadmap to resolve the identified architectural and security issues.

### Phase 1: Security & Sandboxing (Urgent)
* [ ] **SSRF DNS Hook**: Replace pre-request hostname string validation with connection-level IP verification. Configure the `reqwest` client builder with a custom socket connector or resolve the hostname and validate all resolved IPs against the blacklist.
* [ ] **Integrate `url` Crate**: Delete the custom `url_host` parser in `builtins.rs` and utilize the `url` crate.

### Phase 2: Virtual Machine Security (High)
* [ ] **Bytecode Stack Verifier**: Implement a stack depth verifier in `serialize.rs`. This should run before executing or saving any bytecode, validating that the stack does not underflow or overflow for all code paths.
* [ ] **Safe `pop_unchecked`**: Temporarily add bounds checks in release mode to `pop_unchecked` (or replace with standard `.pop()`) until the verifier guarantees safety.

### Phase 3: Workspace Hygiene & Modularity (Medium)
* [ ] **Unify Subcrate Dependency Definitions**: Refactor `sema-wasm/Cargo.toml` and `sema-notebook/Cargo.toml` to inherit versions and path dependencies from `[workspace.dependencies]`.
* [ ] **Split Monolithic Files**:
  * Deconstruct `sema-llm/src/builtins.rs` into `providers/`, `budget.rs`, and `security.rs`.
  * Divide `sema-lsp/src/lib.rs` into `handlers/` (completion, formatting, etc.), `state.rs`, and `server.rs`.
* [ ] **Scoping allowances**: Restrict `#![allow(clippy::mutable_key_type)]` to specific map declarations where safety is commented.

### Phase 4: Evaluator Convergence (Medium)
* [ ] **Closure HOF Upvalue Fix**: Modify HOF closure execution to pass pointers/references to the mutable stack slots (e.g. using cell indicators) rather than cloning snapshot upvalues.
* [ ] **VM Stack Traces**: Implement trace mapping for caught errors inside the VM interpreter.

---

## 7. DAP (Debug Adapter Protocol) Architecture & Deadlock Resolution

The Debug Adapter Protocol (DAP) implementation (`sema-dap`) enables interactive debugging of Sema programs under the bytecode VM using DAP clients (e.g., VS Code or editor plugins).

### 7.1 The Pre-Configuration Deadlock Condition
During initial integration testing, a race condition and deadlock were identified in the message loop between the async DAP server frontend and the execution thread.
* **The Symptom**: When a DAP client configured breakpoints *after* sending `launch` but *before* sending `configurationDone` (which is standard behavior for many editors), the entire DAP session hung indefinitely.
* **The Root Cause**: 
  1. Once `launch` is processed, a command sender channel `dbg_cmd_tx` is initialized and stored in the server state.
  2. The server previously assumed that `dbg_cmd_tx.is_some()` meant the VM debugger loop was active and responsive.
  3. Consequently, requests like `setBreakpoints`, `stackTrace`, `scopes`, or `variables` were forwarded to the VM via `DebugCommand` sync/oneshot channels.
  4. However, the VM does not actually spawn the execution loop or poll its command channel until `configurationDone` is received.
  5. The server blocked on `reply_rx.recv()`, waiting for a response that would never arrive because the VM was not running yet.

### 7.2 The Resolution (`vm_active` Tracking)
To resolve the deadlock, we decoupled VM command routing from channel presence by tracking the VM's lifecycle state:
1. **State Tracking**: Added a `vm_active: bool` flag to the DAP event loop.
2. **Activation**: The flag is set to `true` when the client sends the `configurationDone` request (which executes the VM via `execute_debug`).
3. **Deactivation**: The flag is reset to `false` when the VM finishes execution and sends a `DebugEvent::Terminated` event.
4. **Conditional Routing**:
   * **`setBreakpoints`**: If `vm_active` is `false`, breakpoints are routed to the backend thread via `BackendRequest::SetBreakpoints` to be stored as pending breakpoints. If `vm_active` is `true`, they are sent to the running VM via `DebugCommand::SetBreakpoints`.
   * **`stackTrace` / `scopes` / `variables`**: If `vm_active` is `false` (e.g. before execution starts or after it finishes), the server responds immediately with empty/default arrays instead of waiting on the unresponsive VM channel.

### 7.3 Verification
A new integration test `test_dap_breakpoint_after_launch` was added to [crates/sema-dap/tests/integration_test.rs](file:///Users/helge/code/sema-lisp/crates/sema-dap/tests/integration_test.rs#L317-L400). It explicitly sets a breakpoint between the `launch` and `configurationDone` commands using a strict 2-second timeout helper `read_dap_timeout` to guard against regressions.

### 7.4 Unimplemented / Mocked Features and Breakpoint Matching Limitations
The DAP implementation remains lightweight, and contains several unimplemented, mocked-out, or partially implemented behaviors:
1. **IntelliJ `stopOnEntry` Hardcoding**: The IntelliJ plugin's DAP configuration (`SemaDebugAdapterDescriptorFactory.kt`) hardcodes `stopOnEntry` to `true`. This causes the DAP server to launch the VM with `StepMode::StepInto`, prompting it to stop at the very first expression, even if the user didn't request a halt there.
2. **Path/URI Format Mismatch**: IDEs/DAP clients (like IntelliJ/LSP4IJ) often transmit breakpoint source paths as `file://` URIs (e.g., `file:///path/to/file.sema`), while the compiler maps local filesystem paths (e.g. `/path/to/file.sema`). This path mismatch caused breakpoints to never be matched at runtime, letting execution run straight through. *Note: We have resolved this by introducing path cleaning (`clean_path`) in `server.rs`.*
3. **No Breakpoint Verification or Sliding**: The DAP server blindly responds with `"verified": true` to all breakpoints without checking whether executable instructions actually exist on the requested line. If a breakpoint is placed on a blank line, comment, or non-spanned code boundary (like a closing bracket), it is shown as verified in the IDE but will never be hit by the VM.
4. **Tree-Walker Code Bypasses Debugger**: Code loaded or imported dynamically via `(load ...)` or `(import ...)` is executed using the tree-walking evaluator, completely bypassing the VM bytecode execution loop and debugger command channel. Breakpoints set in dynamically loaded files will never be hit.
5. **Mocked Threads**: The `threads` request returns a single mocked main thread (`{ "id": 1, "name": "main" }`).
6. **Unsupported Requests**: The `evaluate` command (watches, debug console evaluations, hovers to inspect values) is unsupported, returning an error. Mutation of variables from the debugger (`setVariable`) is also not supported.
