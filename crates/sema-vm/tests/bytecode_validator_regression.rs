//! Regression tests for audit finding C11: the `.semac` deserializer does not
//! verify stack balance, but `vm.rs` relies on stack-balanced bytecode via
//! `pop_unchecked`. A crafted `.semac` with leading `Pop` (or any other
//! underflowing sequence) causes UB in release builds.
//!
//! These tests are **ignored** until the stack-depth verifier lands. See:
//!   - `agents/LIMITATIONS.md` #32
//!   - `agents/DECISIONS.md` ADR #56
//!   - FIXME(C11) comment above `pop_unchecked` in `crates/sema-vm/src/vm.rs`
//!
//! When the verifier is implemented, un-ignore these tests; they should pass
//! by causing `deserialize_compile_result` to return a `SemaError` instead of
//! producing a `CompileResult` that the VM will accept.

/// A `.semac` chunk whose first opcode is `Pop` should be rejected at load
/// time, not executed. As of today the deserializer accepts it; the VM then
/// invokes `pop_unchecked` on an empty stack and triggers UB.
///
/// TODO: this test currently has no body — implementing it requires either
///   (a) hand-crafting the `.semac` byte stream that decodes to a chunk with
///       leading `Pop`, or
///   (b) building a CompileResult programmatically and round-tripping it
///       through `serialize_compile_result` / `deserialize_compile_result`
///       after the verifier exists to reject it.
/// Once ADR #56 lands and exposes the verifier entry point, replace this
/// stub with the actual rejection assertion.
#[test]
#[ignore = "C11: bytecode validator — see agents/LIMITATIONS.md #32"]
fn semac_leading_pop_rejected_at_load() {
    // TODO(C11): craft a CompileResult with a chunk that starts with `Op::Pop`,
    // serialize it via `serialize_compile_result`, then attempt to deserialize
    // and assert the result is `Err(_)` with a stack-underflow message.
    //
    // Pseudocode (depends on ADR #56 exposing `validate_stack_balance`):
    //
    //   let bad = CompileResult {
    //       main_chunk: Chunk { code: vec![Op::Pop as u8], ... },
    //       ...
    //   };
    //   let bytes = serialize_compile_result(&bad).unwrap();
    //   let err = deserialize_compile_result(&bytes).unwrap_err();
    //   assert!(err.to_string().contains("stack underflow"),
    //           "expected stack-underflow rejection, got: {err}");
    panic!("stub — verifier not yet implemented (see ADR #56)");
}

/// Same idea, but for an unbalanced sequence in the middle of a function:
/// `Const 0; Pop; Pop` underflows on the second `Pop`. The verifier should
/// reject this via abstract interpretation of stack depth per instruction.
#[test]
#[ignore = "C11: bytecode validator — see agents/LIMITATIONS.md #32"]
fn semac_mid_chunk_underflow_rejected() {
    // TODO(C11): see notes on `semac_leading_pop_rejected_at_load`.
    panic!("stub — verifier not yet implemented (see ADR #56)");
}

/// A function whose body does not leave exactly one value on the stack at
/// `Return` should also be rejected (e.g. `Return` with depth 0 or 2+).
#[test]
#[ignore = "C11: bytecode validator — see agents/LIMITATIONS.md #32"]
fn semac_unbalanced_return_rejected() {
    // TODO(C11): see notes on `semac_leading_pop_rejected_at_load`.
    panic!("stub — verifier not yet implemented (see ADR #56)");
}
