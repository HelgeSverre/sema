# VM Fuzzing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add fuzzing infrastructure for the bytecode VM (`sema-vm`) to find panics, UB, correctness bugs, and DoS vectors.

**Architecture:** Three-phase approach: (1) harden the VM against UB and add a fuel limit so fuzzing is safe and terminates, (2) add source-level fuzz targets (robustness + differential), (3) add raw-bytecode fuzz target for deep VM crash testing. Each phase builds on the previous.

**Tech Stack:** `cargo-fuzz` / `libfuzzer-sys`, nightly Rust, `arbitrary` crate (for raw bytecode target).

**Prerequisites:** The VM should be functionally complete enough that differential comparison against the tree-walker is meaningful (most special forms compiled and executing correctly).

---

## Phase 1: Harden VM for Fuzzing

### Task 1: Replace `unsafe transmute` with safe opcode decoding

**Files:**

- Modify: `crates/sema-vm/src/opcodes.rs`
- Modify: `crates/sema-vm/src/vm.rs:86`
- Test: `crates/sema-vm/src/vm.rs` (existing tests must still pass)

**Context:** Line 86 of `vm.rs` does `unsafe { std::mem::transmute::<u8, Op>(code[pc]) }` which is **undefined behavior** for any byte value that isn't a valid `Op` discriminant (0..=41). This must be fixed before fuzzing can produce trustworthy results.

**Step 1: Add `Op::from_byte` safe decoder**

Add a `from_byte` method to `Op` in `opcodes.rs`:

```rust
impl Op {
    /// Decode a byte into an opcode. Returns None for invalid/unknown bytes.
    pub fn from_byte(b: u8) -> Option<Op> {
        if b <= Op::EqInt as u8 {
            // SAFETY: We just verified b is in the valid discriminant range.
            // All variants from 0..=max are contiguous (no gaps) due to #[repr(u8)].
            Some(unsafe { std::mem::transmute::<u8, Op>(b) })
        } else {
            None
        }
    }
}
```

**Step 2: Update `vm.rs` dispatch to use safe decode**

Replace the transmute in `vm.rs:86`:

```rust
// Before:
let op = unsafe { std::mem::transmute::<u8, Op>(code[pc]) };

// After:
let op = Op::from_byte(code[pc])
    .ok_or_else(|| SemaError::eval(format!("VM: invalid opcode 0x{:02x} at pc={}", code[pc], pc)))?;
```

**Step 3: Run existing tests**

Run: `cargo test -p sema-vm`
Expected: All tests PASS (no behavior change for valid bytecode).

**Step 4: Commit**

```bash
git add crates/sema-vm/src/opcodes.rs crates/sema-vm/src/vm.rs
git commit -m "fix: replace unsafe transmute with safe opcode decoding"
```

---

### Task 2: Add VM fuel / step limit

**Files:**

- Modify: `crates/sema-vm/src/vm.rs` (add fuel counter to `VM` struct and decrement in `run()` loop)
- Test: `crates/sema-vm/src/vm.rs` (add unit test for fuel exhaustion)

**Context:** The tree-walker fuzzer uses `FUZZ_STEP_LIMIT = 100_000` to prevent infinite loops. The VM has no equivalent — a `Jump -5` instruction creates an infinite loop that hangs the fuzzer forever.

**Step 1: Add fuel field to VM**

```rust
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: Rc<Env>,
    functions: Vec<Rc<Function>>,
    fuel: Option<usize>,  // None = unlimited, Some(n) = steps remaining
}
```

Update `VM::new` to set `fuel: None`.

Add a public setter:

```rust
impl VM {
    pub fn set_fuel(&mut self, fuel: usize) {
        self.fuel = Some(fuel);
    }
}
```

**Step 2: Decrement fuel in the dispatch loop**

At the top of the `loop` in `run()`, before reading the opcode:

```rust
if let Some(ref mut fuel) = self.fuel {
    if *fuel == 0 {
        return Err(SemaError::eval("VM: fuel exhausted"));
    }
    *fuel -= 1;
}
```

**Step 3: Write a unit test for fuel exhaustion**

```rust
#[test]
fn test_vm_fuel_exhaustion() {
    let globals = make_test_env();
    let ctx = EvalContext::new();
    // Infinite loop via named let
    let input = "(let loop () (loop))";
    let vals = sema_reader::read_many(input).unwrap();
    let mut resolved = Vec::new();
    let mut total_locals: u16 = 0;
    for val in &vals {
        let core = crate::lower::lower(val).unwrap();
        let (res, n) = crate::resolve::resolve_with_locals(&core).unwrap();
        total_locals = total_locals.max(n);
        resolved.push(res);
    }
    let result = crate::compiler::compile_many_with_locals(&resolved, total_locals).unwrap();
    let functions: Vec<Rc<Function>> = result.functions.into_iter().map(Rc::new).collect();
    let closure = Rc::new(Closure {
        func: Rc::new(Function {
            name: None,
            chunk: result.chunk,
            upvalue_descs: Vec::new(),
            arity: 0,
            has_rest: false,
            local_names: Vec::new(),
        }),
        upvalues: Vec::new(),
    });
    let mut vm = VM::new(Rc::new(globals), functions);
    vm.set_fuel(1000);
    let err = vm.execute(closure, &ctx).unwrap_err();
    assert!(err.to_string().contains("fuel exhausted"));
}
```

**Step 4: Run tests**

Run: `cargo test -p sema-vm`
Expected: All tests PASS including the new fuel test.

**Step 5: Commit**

```bash
git add crates/sema-vm/src/vm.rs
git commit -m "feat: add fuel/step limit to VM for fuzzing safety"
```

---

### Task 3: Add bounds checks to operand reads

**Files:**

- Modify: `crates/sema-vm/src/vm.rs` (`read_u16`, `read_u32`, `read_i32`, `read_spur`)

**Context:** `read_u16` reads `code[pc+1]` and `code[pc+2]` without checking bounds. If a truncated instruction is at the end of `code`, this panics with index-out-of-bounds. Same for `read_u32`/`read_i32`. Also `read_spur` transmutes `u32→Spur` which may be UB for zero values.

**Step 1: Make operand reads return Result**

Convert `read_u16`, `read_u32`, `read_i32` to return `Result<_, SemaError>`:

```rust
fn read_u16(&mut self) -> Result<u16, SemaError> {
    let frame = self.frames.last_mut().unwrap();
    let code = &frame.closure.func.chunk.code;
    let pc = frame.pc + 1;
    if pc + 2 > code.len() {
        return Err(SemaError::eval("VM: truncated operand (u16)"));
    }
    let val = u16::from_le_bytes([code[pc], code[pc + 1]]);
    frame.pc = pc + 2;
    Ok(val)
}
```

Apply the same pattern to `read_u32` (check `pc + 4`) and `read_i32` (check `pc + 4`).

For `read_spur`, add a zero check:

```rust
fn read_spur(&mut self) -> Result<Spur, SemaError> {
    let bits = self.read_u32()?;
    if bits == 0 {
        return Err(SemaError::eval("VM: invalid spur (zero)"));
    }
    Ok(unsafe { std::mem::transmute::<u32, Spur>(bits) })
}
```

**Step 2: Update all call sites**

Every `self.read_u16()` becomes `self.read_u16()?`, etc. This affects:

- `Op::Const`, `Op::LoadLocal`, `Op::StoreLocal`, `Op::LoadUpvalue`, `Op::StoreUpvalue`
- `Op::Call`, `Op::TailCall`, `Op::MakeList`, `Op::MakeVector`, `Op::MakeMap`, `Op::MakeHashMap`
- `Op::CallNative`, `Op::MakeClosure`
- `Op::LoadGlobal`, `Op::StoreGlobal`, `Op::DefineGlobal` (via `read_spur`)
- `Op::Jump`, `Op::JumpIfFalse`, `Op::JumpIfTrue` (via `read_i32`)

**Step 3: Run tests**

Run: `cargo test -p sema-vm`
Expected: All tests PASS.

**Step 4: Commit**

```bash
git add crates/sema-vm/src/vm.rs
git commit -m "fix: add bounds checks to VM operand reads"
```

---

## Phase 2: Source-Level Fuzz Targets

### Task 4: Set up `cargo-fuzz` for `sema-vm`

**Files:**

- Create: `crates/sema-vm/fuzz/Cargo.toml`
- Create: `crates/sema-vm/fuzz/fuzz_targets/fuzz_vm.rs`
- Modify: `Makefile` (add `fuzz-vm` target)

**Context:** Mirror the existing `sema-eval/fuzz` structure. This target feeds source strings through `eval_str` and asserts that it never panics (Ok or Err both fine, panic = bug).

**Step 1: Create `fuzz/Cargo.toml`**

```toml
[package]
name = "sema-vm-fuzz"
version = "0.0.0"
publish = false
edition = "2021"

[package.metadata]
cargo-fuzz = true

[dependencies]
libfuzzer-sys = "0.4"

[dependencies.sema-core]
path = "../../sema-core"

[dependencies.sema-reader]
path = "../../sema-reader"

[dependencies.sema-vm]
path = ".."

[dependencies.sema-stdlib]
path = "../../sema-stdlib"

# Prevent this from interfering with workspaces
[workspace]
members = ["."]

[[bin]]
name = "fuzz_vm"
path = "fuzz_targets/fuzz_vm.rs"
doc = false
```

**Step 2: Create fuzz target `fuzz_vm.rs`**

```rust
#![no_main]
use libfuzzer_sys::fuzz_target;
use std::rc::Rc;

/// Max VM steps per fuzz input — prevents infinite loops from timing out.
const FUZZ_FUEL: usize = 100_000;

thread_local! {
    /// Reuse a single stdlib environment across fuzz iterations.
    static ENV: Rc<sema_core::Env> = {
        let env = sema_core::Env::new();
        sema_stdlib::register_stdlib(&env);
        Rc::new(env)
    };
}

fuzz_target!(|data: &[u8]| {
    let input = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    ENV.with(|global_env| {
        let env = Rc::new(sema_core::Env::with_parent(global_env.clone()));
        let ctx = sema_core::EvalContext::new();

        // Parse
        let vals = match sema_reader::read_many(input) {
            Ok(v) => v,
            Err(_) => return,
        };

        // Lower + resolve + compile
        let mut resolved = Vec::new();
        let mut total_locals: u16 = 0;
        for val in &vals {
            let core = match sema_vm::lower(val) {
                Ok(c) => c,
                Err(_) => return,
            };
            let (res, n) = match sema_vm::resolve_with_locals(&core) {
                Ok(r) => r,
                Err(_) => return,
            };
            total_locals = total_locals.max(n);
            resolved.push(res);
        }

        let result = match sema_vm::compile_many_with_locals(&resolved, total_locals) {
            Ok(r) => r,
            Err(_) => return,
        };

        let functions: Vec<Rc<sema_vm::Function>> =
            result.functions.into_iter().map(Rc::new).collect();
        let closure = Rc::new(sema_vm::Closure {
            func: Rc::new(sema_vm::Function {
                name: None,
                chunk: result.chunk,
                upvalue_descs: Vec::new(),
                arity: 0,
                has_rest: false,
                local_names: Vec::new(),
            }),
            upvalues: Vec::new(),
        });

        let mut vm = sema_vm::VM::new(env, functions);
        vm.set_fuel(FUZZ_FUEL);
        // Must not panic — Ok or Err are both fine
        let _ = vm.execute(closure, &ctx);
    });
});
```

**Step 3: Add Makefile target**

Add after the existing `fuzz-eval` target:

```makefile
fuzz-vm:
	cd crates/sema-vm && rustup run nightly cargo fuzz run fuzz_vm -- -max_total_time=120 -timeout=10
```

Update the `fuzz:` umbrella target:

```makefile
fuzz: fuzz-reader fuzz-eval fuzz-vm
```

**Step 4: Verify it builds and runs briefly**

Run: `cd crates/sema-vm && rustup run nightly cargo fuzz run fuzz_vm -- -max_total_time=5`
Expected: Runs for 5 seconds without crashing the harness itself. May or may not find bugs.

**Step 5: Commit**

```bash
git add crates/sema-vm/fuzz/ Makefile
git commit -m "feat: add source-level fuzz target for VM"
```

---

### Task 5: Add differential fuzz target (tree-walker vs VM)

**Files:**

- Create: `crates/sema-vm/fuzz/fuzz_targets/fuzz_vm_diff.rs`
- Modify: `crates/sema-vm/fuzz/Cargo.toml` (add `sema-eval` dependency + new `[[bin]]`)
- Modify: `Makefile` (add `fuzz-vm-diff` target)

**Context:** This is the highest-value target. It runs the same source string through both the tree-walker (`sema_eval::eval_string`) and the VM (`sema_vm::eval_str`), then compares results. A mismatch where both succeed but produce different values is a correctness bug.

**Step 1: Add `sema-eval` dependency to fuzz Cargo.toml**

```toml
[dependencies.sema-eval]
path = "../../sema-eval"
```

Add new bin:

```toml
[[bin]]
name = "fuzz_vm_diff"
path = "fuzz_targets/fuzz_vm_diff.rs"
doc = false
```

**Step 2: Create `fuzz_vm_diff.rs`**

```rust
#![no_main]
use libfuzzer_sys::fuzz_target;
use sema_core::{Env, EvalContext, Value};
use std::rc::Rc;

const FUZZ_FUEL: usize = 50_000;
const FUZZ_EVAL_STEP_LIMIT: usize = 50_000;

thread_local! {
    static ENV: Rc<Env> = {
        let env = Env::new();
        sema_stdlib::register_stdlib(&env);
        sema_eval::set_eval_step_limit(FUZZ_EVAL_STEP_LIMIT);
        Rc::new(env)
    };
}

/// Normalize values for comparison (handle known differences).
fn normalize(val: &Value) -> String {
    // Use debug repr for comparison — covers most types
    format!("{:?}", val)
}

fuzz_target!(|data: &[u8]| {
    let input = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Skip inputs that use features with known differences or side effects
    if input.contains("load")
        || input.contains("import")
        || input.contains("display")
        || input.contains("print")
        || input.contains("file/")
        || input.contains("llm")
        || input.contains("defmacro")
        || input.contains("macroexpand")
        || input.contains("eval ")  // meta-eval has different semantics
        || input.contains("gensym")
        || input.contains("random")
    {
        return;
    }

    ENV.with(|global_env| {
        // Tree-walker
        let eval_env = Env::with_parent(global_env.clone());
        let eval_result = sema_eval::eval_string(input, &eval_env);

        // VM
        let vm_env = Rc::new(Env::with_parent(global_env.clone()));
        let ctx = EvalContext::new();
        let vm_result = sema_vm::vm::eval_str(input, &vm_env, &ctx);

        match (eval_result, vm_result) {
            (Ok(eval_val), Ok(vm_val)) => {
                let eval_norm = normalize(&eval_val);
                let vm_norm = normalize(&vm_val);
                assert_eq!(
                    eval_norm, vm_norm,
                    "MISMATCH on input {:?}\n  eval: {:?}\n  vm:   {:?}",
                    input, eval_val, vm_val
                );
            }
            // Both error → fine (error messages may differ)
            (Err(_), Err(_)) => {}
            // One succeeds, one fails → might be a bug, but could also be
            // a feature gap. Log for triage but don't assert.
            // TODO: Once VM is feature-complete, make this an assertion.
            _ => {}
        }
    });
});
```

**Step 3: Add Makefile target**

```makefile
fuzz-vm-diff:
	cd crates/sema-vm && rustup run nightly cargo fuzz run fuzz_vm_diff -- -max_total_time=120 -timeout=10
```

**Step 4: Verify it builds**

Run: `cd crates/sema-vm && rustup run nightly cargo fuzz build`
Expected: Both `fuzz_vm` and `fuzz_vm_diff` compile successfully.

**Step 5: Commit**

```bash
git add crates/sema-vm/fuzz/ Makefile
git commit -m "feat: add differential fuzz target (tree-walker vs VM)"
```

---

## Phase 3: Raw Bytecode Fuzzing (Advanced)

### Task 6: Add raw bytecode fuzz target

**Files:**

- Create: `crates/sema-vm/fuzz/fuzz_targets/fuzz_vm_bytecode.rs`
- Modify: `crates/sema-vm/fuzz/Cargo.toml` (add `arbitrary` dependency + new `[[bin]]`)
- Modify: `Makefile` (add `fuzz-vm-bytecode` target)

**Context:** This bypasses the compiler entirely and feeds crafted bytecode directly to `VM::execute`. It's the best way to find panics in the VM dispatch loop (stack underflow, OOB index, invalid operands). Requires Phase 1 hardening to be complete.

**Step 1: Add `arbitrary` dependency**

In `crates/sema-vm/fuzz/Cargo.toml`:

```toml
[dependencies]
libfuzzer-sys = { version = "0.4", features = ["arbitrary-derive"] }
arbitrary = { version = "1", features = ["derive"] }
```

Add new bin:

```toml
[[bin]]
name = "fuzz_vm_bytecode"
path = "fuzz_targets/fuzz_vm_bytecode.rs"
doc = false
```

**Step 2: Create `fuzz_vm_bytecode.rs`**

```rust
#![no_main]
use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;
use sema_core::{Env, EvalContext, Value};
use sema_vm::{Chunk, Closure, ExceptionEntry, Function, UpvalueDesc, VM};
use std::rc::Rc;

const MAX_CODE_LEN: usize = 512;
const MAX_CONSTS: usize = 16;
const MAX_LOCALS: u16 = 32;
const MAX_FUNCTIONS: usize = 4;
const FUZZ_FUEL: usize = 10_000;

/// A fuzzable bytecode program.
#[derive(Debug, Arbitrary)]
struct FuzzProgram {
    code: Vec<u8>,
    n_consts: u8,
    n_locals: u8,
    n_functions: u8,
}

/// Generate simple constant values for the pool.
fn make_const(seed: u8) -> Value {
    match seed % 5 {
        0 => Value::Nil,
        1 => Value::Bool(seed > 128),
        2 => Value::Int(seed as i64 - 128),
        3 => Value::Float(seed as f64 / 10.0),
        4 => Value::String(Rc::new(format!("s{}", seed))),
        _ => Value::Nil,
    }
}

fuzz_target!(|data: &[u8]| {
    let mut u = Unstructured::new(data);
    let prog: FuzzProgram = match FuzzProgram::arbitrary(&mut u) {
        Ok(p) => p,
        Err(_) => return,
    };

    // Bound sizes
    let code = if prog.code.len() > MAX_CODE_LEN {
        &prog.code[..MAX_CODE_LEN]
    } else {
        &prog.code
    };
    if code.is_empty() {
        return;
    }

    let n_consts = (prog.n_consts as usize).min(MAX_CONSTS);
    let n_locals = (prog.n_locals as u16).min(MAX_LOCALS);
    let n_functions = (prog.n_functions as usize).min(MAX_FUNCTIONS);

    // Build constant pool
    let consts: Vec<Value> = (0..n_consts)
        .map(|i| make_const(i as u8))
        .collect();

    // Build sub-functions (minimal: just Return)
    let mut functions: Vec<Rc<Function>> = Vec::new();
    for _ in 0..n_functions {
        let sub_chunk = Chunk {
            code: vec![sema_vm::Op::Nil as u8, sema_vm::Op::Return as u8],
            consts: vec![],
            spans: vec![],
            max_stack: 8,
            n_locals: 0,
            exception_table: vec![],
        };
        functions.push(Rc::new(Function {
            name: None,
            chunk: sub_chunk,
            upvalue_descs: vec![],
            arity: 0,
            has_rest: false,
            local_names: vec![],
        }));
    }

    let chunk = Chunk {
        code: code.to_vec(),
        consts,
        spans: vec![],
        max_stack: 64,
        n_locals,
        exception_table: vec![],
    };

    let closure = Rc::new(Closure {
        func: Rc::new(Function {
            name: None,
            chunk,
            upvalue_descs: vec![],
            arity: 0,
            has_rest: false,
            local_names: vec![],
        }),
        upvalues: vec![],
    });

    let env = Rc::new(Env::new());
    let ctx = EvalContext::new();
    let mut vm = VM::new(env, functions);
    vm.set_fuel(FUZZ_FUEL);

    // Must not panic — Ok or Err are both fine
    let _ = vm.execute(closure, &ctx);
});
```

**Step 3: Add Makefile target**

```makefile
fuzz-vm-bytecode:
	cd crates/sema-vm && rustup run nightly cargo fuzz run fuzz_vm_bytecode -- -max_total_time=120 -timeout=10
```

**Step 4: Verify it builds**

Run: `cd crates/sema-vm && rustup run nightly cargo fuzz build`
Expected: All three fuzz targets compile.

**Step 5: Commit**

```bash
git add crates/sema-vm/fuzz/ Makefile
git commit -m "feat: add raw bytecode fuzz target for VM"
```

---

## Phase 4: Seed Corpus & CI Integration (Optional)

### Task 7: Generate seed corpus from compiled examples

**Files:**

- Create: `crates/sema-vm/fuzz/generate_corpus.rs` (helper binary or test)
- Create: `crates/sema-vm/fuzz/corpus/fuzz_vm/` (directory with seed inputs)

**Context:** libFuzzer works better with a seed corpus. For the source-level targets, seed with `.sema` example files. For the bytecode target, compile examples and serialize the chunk bytes.

**Step 1: Create corpus directory with hand-picked source inputs**

Create `crates/sema-vm/fuzz/corpus/fuzz_vm/` with files containing representative Sema programs:

- `basic.sema`: `42`
- `arith.sema`: `(+ 1 2)`
- `if.sema`: `(if #t 1 2)`
- `let.sema`: `(let ((x 10) (y 20)) (+ x y))`
- `lambda.sema`: `((lambda (x) (* x x)) 5)`
- `closure.sema`: `(let ((x 10)) ((lambda () x)))`
- `do.sema`: `(do ((i 0 (+ i 1))) ((= i 5) i))`
- `try.sema`: `(try (throw "boom") (catch e e))`
- `named-let.sema`: `(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))`
- `nested.sema`: `(let ((f (lambda (x) (lambda (y) (+ x y))))) ((f 10) 20))`
- `and-or.sema`: `(and (or #f 1) (and 2 3))`
- `list.sema`: `(list 1 2 3)`
- `vector.sema`: `[1 2 3]`
- `map.sema`: `{:a 1 :b 2}`

**Step 2: Commit**

```bash
git add crates/sema-vm/fuzz/corpus/
git commit -m "feat: add seed corpus for VM fuzzing"
```

---

### Task 8: Add additional VM stack/frame guards for robustness

**Files:**

- Modify: `crates/sema-vm/src/vm.rs`

**Context:** Beyond fuel limits, add guards against pathological resource usage that could OOM the fuzzer.

**Step 1: Add max stack and max frame depth constants**

```rust
const MAX_STACK_SIZE: usize = 65_536;
const MAX_FRAMES: usize = 1_024;
```

**Step 2: Check stack size before push-heavy ops**

In the `run()` loop, add a periodic stack size check (e.g., after each `push` or before `MakeList/Vector/Map`):

```rust
// Before MakeList/MakeVector/MakeMap/MakeHashMap operations:
if self.stack.len() > MAX_STACK_SIZE {
    return Err(SemaError::eval("VM: stack overflow"));
}
```

**Step 3: Check frame depth before Call**

In `call_value`, before pushing a new frame:

```rust
if self.frames.len() >= MAX_FRAMES {
    return Err(SemaError::eval("VM: call stack overflow"));
}
```

**Step 4: Run tests**

Run: `cargo test -p sema-vm`
Expected: All tests PASS (limits are high enough for normal programs).

**Step 5: Commit**

```bash
git add crates/sema-vm/src/vm.rs
git commit -m "feat: add stack/frame depth limits to VM"
```

---

## Summary of expected bugs

| Bug class                                   | Found by                       | Severity |
| ------------------------------------------- | ------------------------------ | -------- |
| UB from `transmute` on invalid opcode bytes | Task 1 (fix) + Task 6 (verify) | Critical |
| Stack underflow panics (`pop().unwrap()`)   | Task 4, Task 6                 | High     |
| OOB reads in operand decoding               | Task 3 (fix) + Task 6 (verify) | High     |
| Correctness: wrong result from compiler     | Task 5 (differential)          | High     |
| Jump offset wrapping → infinite loop        | Task 2 (fuel) + Task 6         | Medium   |
| `functions[func_id]` OOB in MakeClosure     | Task 6                         | Medium   |
| Exception handler stack corruption          | Task 6                         | Medium   |
| Closure/upvalue aliasing bugs               | Task 5 (differential)          | Medium   |
| OOM from unbounded containers               | Task 8                         | Low      |
