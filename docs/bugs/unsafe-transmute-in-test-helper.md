# Unsafe Transmute in Test Helper `extract_ops`

## Problem

The `extract_ops` helper function in the VM compiler tests uses `unsafe { std::mem::transmute::<u8, Op>(code[pc]) }` to convert raw bytecode bytes back into `Op` enum variants. This is undefined behavior if the byte value does not correspond to a valid `Op` discriminant.

When new opcodes are added to the `Op` enum, `extract_ops` will still transmute the new byte values, but if the match arms in `extract_ops` are not updated to handle the new opcode's operand layout, it will silently misparse the bytecode stream -- reading operand bytes as opcodes or vice versa. This produces garbage output rather than a clear error, making debugging difficult.

## Affected Tests

- `crates/sema-vm/src/compiler.rs` -- `extract_ops` function -- uses `unsafe { std::mem::transmute::<u8, Op>(code[pc]) }` to decode bytecode
- All compiler unit tests that call `extract_ops` to verify emitted bytecode

## Suggested Fix

1. Replace `unsafe { std::mem::transmute }` with a safe `Op::try_from(u8)` conversion using the `TryFrom` trait. Derive or implement `TryFrom<u8>` for `Op` that returns an error for invalid discriminants.
2. Add a compile-time exhaustiveness check: a `#[test]` that asserts `std::mem::size_of::<Op>()` or uses a const assertion to catch when new variants are added.
3. Alternatively, use the `num_enum` crate which provides `TryFromPrimitive` derive for enums, giving safe conversion with clear error messages.
4. At minimum, make the match in `extract_ops` non-exhaustive with a panic arm: `_ => panic!("unknown opcode {}", code[pc])` so failures are loud rather than silent.
