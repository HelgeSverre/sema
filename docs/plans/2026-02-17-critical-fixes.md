# Critical Fixes — Code Review Findings

Date: 2026-02-17

## Summary

A critical review of Sema's internals revealed several issues ranging from security holes to undefined behavior risks. This document tracks all findings, prioritized by severity.

## 🔴 Critical (Fixed)

### 1. Sandbox bypass in VM delegates
**Status: FIXED**

`__vm-load` and `__vm-import` called `std::fs::read_to_string` directly without checking `ctx.sandbox.check(Caps::FS_READ, ...)`. Running `--vm --sandbox=strict` allowed arbitrary file reads.

**Fix:** Added `ctx.sandbox.check(Caps::FS_READ, "load")` and `ctx.sandbox.check(Caps::FS_READ, "import")` to both VM delegate functions in `eval.rs`.

### 2. VM opcode dispatch uses magic numbers instead of `Op` enum
**Status: FIXED**

The VM `run()` loop matched on raw `u8` values (`0`, `1`, `2`, ...) with comments like `0 /* Const */`. The `Op` enum existed with a `from_u8()` method but wasn't used. Any opcode renumbering would silently break the VM.

**Fix:** Replaced all 42 magic number arms with `Op` enum constants (e.g., `Op::Const as u8 =>`). The compiler now catches any mismatch.

### 3. NaN-boxing has no compile-time platform guard
**Status: FIXED**

`ptr_to_payload` had `debug_assert!(raw >> 48 == 0)` but this only fires in debug builds. On platforms with >48-bit virtual addresses, pointers would silently truncate in release mode → UB.

**Fix:** Added `const _: ()` compile-time assertion in `value.rs` that refuses to compile on non-64-bit platforms or platforms where pointer width exceeds what NaN-boxing can encode.

### 4. VM uses `unsafe` for no measurable benefit
**Status: FIXED**

The dispatch loop used `unsafe { get_unchecked(fi) }` and a raw pointer cast to borrow the code slice. If `frames` was ever empty (error path), this would be UB. The pointer cast was completely unnecessary — the borrow already existed.

**Fix:** Replaced `unsafe { get_unchecked(fi) }` with safe indexing. Removed the raw pointer cast. The code slice is now borrowed safely.

## 🟠 Serious (Tracked — Future Work)

### 5. Thread-local callback architecture
`set_eval_callback`/`set_call_callback` are global TLS. Creating two `Interpreter` instances in one thread silently overwrites callbacks. This makes embedding Sema as a library a footgun. **Fix:** Store evaluator/caller in `EvalContext` or pass explicitly.

### 6. LLM domain types in `sema-core`
`Prompt`, `Conversation`, `Agent`, `ToolDefinition`, message roles, images are all in the core `Value` enum. This bloats the tag space and creates coupling between core runtime and LLM concepts. **Fix:** Represent as maps/records or move to a separate `sema-llm-types` crate.

### 7. String interner leaks forever
`INTERNER` is TLS, never evicts. Long-running REPLs accumulate interned strings indefinitely. Not a bug today, but a slow memory leak by design.

### 8. Span table keyed by `Rc` pointer address
`Rc::as_ptr() as usize` as a map key. Pointer addresses can be reused after dealloc. In long sessions, spans can point to wrong expressions. The table also hard-clears at `MAX_SPAN_TABLE_ENTRIES`.

## 🟡 Significant (Tracked — Quality)

### 9. No cross-mode tests (tree-walker vs VM)
No property-based tests verify that tree-walker and VM produce identical results for the same program. When modes diverge, there's no automated detection.

### 10. Stdlib silent error swallowing
`file/list` uses `.filter_map(|e| e.ok())` which silently drops IO errors. Several `defagent` options silently fall back to defaults on type errors instead of reporting.

### 11. `try` catches all error types
`try`/`catch` catches everything including `PermissionDenied`, `Io`, `Unbound` — users can easily swallow system-level errors. Should consider distinguishing user exceptions from interpreter errors.

### 12. `transmute::<u32, Spur>` is fragile
Appears in multiple places. If `lasso` changes internal representation, this breaks silently. Should use safe conversion APIs.
