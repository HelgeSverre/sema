---
outline: [2, 3]
---

# Bytecode File Format (`.semac`)

::: tip Status: Implemented (Alpha)
The `.semac` bytecode file format is implemented and available via `sema compile` and `sema disasm`. The format is not yet stable — breaking changes are expected before v1.0.
:::

## Overview

Sema supports compiling source files to bytecode files (`.semac`) for faster loading and distribution without source. The compilation pipeline is:

```
Source (.sema) → Reader → Lower → Optimize → Resolve → Compile → Serialize → .semac file
```

Loading a `.semac` file skips parsing, lowering, resolution, and compilation — the VM directly deserializes and executes the pre-compiled bytecode.

### CLI Interface

```bash
# Compile a source file to bytecode
sema compile script.sema                   # → script.semac
sema compile -o output.semac script.sema   # explicit output path

# Run a bytecode file (auto-detected via magic number)
sema script.semac

# Validate a bytecode file
sema compile --check script.semac

# Disassemble a bytecode file
sema disasm script.semac
sema disasm --json script.semac            # structured JSON output
```

### Design Goals

1. **Fast loading** — skip parsing and compilation; the primary benefit (like Lua's `luac`)
2. **Source protection** — distribute without revealing source code
3. **Debuggability** — optional debug sections for source maps, local names, breakpoints
4. **Forward compatibility** — version field allows graceful rejection of incompatible bytecode
5. **Simplicity** — flat section-based format, no complex container (no ELF, no zip)

### Non-Goals

- **Portability** — bytecode files are tied to the Sema version that produced them (like Lua). Always keep source files.
- **AOT native compilation** — Sema's dynamic nature (eval, macros, LLM primitives) makes this impractical
- **Streaming** — the entire file is read into memory; no mmap or lazy loading

## File Layout

A `.semac` file consists of a fixed **header**, followed by a sequence of **sections**. Each section has a type tag, length, and payload.

```
┌──────────────────────────────────────┐
│           File Header (24 bytes)     │
├──────────────────────────────────────┤
│  Section: String Table    (required) │
├──────────────────────────────────────┤
│  Section: Function Table  (required) │
├──────────────────────────────────────┤
│  Section: Main Chunk      (required) │
├──────────────────────────────────────┤
│  Section: Source Map      (optional) │
├──────────────────────────────────────┤
│  Section: Debug Symbols   (optional) │
├──────────────────────────────────────┤
│  Section: Breakpoints     (optional) │
├──────────────────────────────────────┤
│  ... future sections ...             │
└──────────────────────────────────────┘
```

All multi-byte integers are **little-endian**. All strings are **UTF-8**.

## File Header

| Offset | Size | Field | Description |
|--------|------|-------|-------------|
| 0 | 4 | `magic` | `\x00SEM` (`0x00`, `0x53`, `0x45`, `0x4D`) |
| 4 | 2 | `format_version` | Bytecode format version (currently `3`) |
| 6 | 2 | `flags` | Bit flags (see below) |
| 8 | 2 | `sema_major` | Sema version major that produced this file |
| 10 | 2 | `sema_minor` | Sema version minor |
| 12 | 2 | `sema_patch` | Sema version patch |
| 14 | 2 | `n_sections` | Number of sections in the file |
| 16 | 4 | `source_hash` | CRC-32 of the original source file (0 if unknown) |
| 20 | 4 | `reserved` | Reserved for future use (must be 0) |

**Total: 24 bytes**

### Magic Number

The magic bytes `\x00SEM` serve two purposes:
1. **File type identification** — the CLI uses this to auto-detect bytecode vs source (source files never start with a null byte)
2. **Corruption detection** — if the magic doesn't match, reject the file immediately

### Flags (Bit Field)

| Bit | Name | Description |
|-----|------|-------------|
| 0 | `HAS_DEBUG` | File contains debug sections (Source Map, Debug Symbols) |
| 1 | `HAS_SOURCE_MAP` | File contains a Source Map section |
| 2 | `HAS_BREAKPOINTS` | File contains a Breakpoints section |
| 3–15 | — | Reserved (must be 0) |

The current serializer always writes `flags = 0` — debug sections (and a `--strip` flag to omit them) are not yet implemented.

## Section Format

Each section begins with a section header:

| Offset | Size | Field | Description |
|--------|------|-------|-------------|
| 0 | 2 | `section_type` | Section type tag (see table) |
| 2 | 4 | `section_length` | Byte length of section payload (excluding this header) |

**Section header: 6 bytes**, followed by `section_length` bytes of payload.

### Section Types

| Type ID | Name | Required | Description |
|---------|------|----------|-------------|
| `0x01` | String Table | ✅ | All interned strings (Spur remapping) |
| `0x02` | Function Table | ✅ | Compiled function templates |
| `0x03` | Main Chunk | ✅ | Top-level bytecode |
| `0x10` | Source Map | — | Source file name + PC-to-line mapping |
| `0x11` | Debug Symbols | — | Local variable names per function |
| `0x12` | Breakpoints | — | Reserved for breakpoint table |
| `0x13` | Debug Scopes | — | Reserved for lexical scope ranges |

Unknown section types are **skipped** (forward compatibility).

## String Table (Section `0x01`)

The string table contains all unique strings referenced by the bytecode, including:
- Symbol names (global identifiers, function names)
- Keyword names
- String constants in the constant pool
- Source file paths (in debug sections)

```
┌────────────────────────────┐
│  count: u32                │  Number of strings
├────────────────────────────┤
│  String Entry 0            │
│    len: u32                │  Byte length of UTF-8 data
│    data: [u8; len]         │  UTF-8 bytes (no null terminator)
├────────────────────────────┤
│  String Entry 1            │
│    ...                     │
└────────────────────────────┘
```

On load, each string is interned into the process-local `lasso::Rodeo` (a thread-local interner), producing a fresh `Spur`. The loader builds a **remap table** (`Vec<Spur>`) mapping file-local string indices to process-local Spurs.

String index `0` is reserved and must be the empty string `""`.

## Main Chunk (Section `0x03`)

The main chunk contains the top-level bytecode and its constant pool.

```
┌────────────────────────────────┐
│  code_len: u32                 │
│  code: [u8; code_len]          │  Raw bytecode
├────────────────────────────────┤
│  n_consts: u16                 │
│  constants: [SerializedValue]  │  Constant pool entries
├────────────────────────────────┤
│  n_spans: u32                  │
│  spans: [(u32 pc, u32 line,    │  PC → source location
│           u32 col, u32         │
│           end_line, u32        │
│           end_col)]            │
├────────────────────────────────┤
│  max_stack: u16                │
│  n_locals: u16                 │
│  n_global_cache_slots: u16     │  Inline cache slots for global lookups
├────────────────────────────────┤
│  n_exceptions: u16             │
│  exceptions: [ExceptionEntry]  │  Exception table
└────────────────────────────────┘
```

### Exception Entry (16 bytes each)

| Offset | Size | Field |
|--------|------|-------|
| 0 | 4 | `try_start` (PC) |
| 4 | 4 | `try_end` (PC) |
| 8 | 4 | `handler_pc` |
| 12 | 2 | `stack_depth` |
| 14 | 2 | `catch_slot` |

## Function Table (Section `0x02`)

```
┌────────────────────────────────┐
│  count: u32                    │  Number of functions
├────────────────────────────────┤
│  Function Entry 0              │
│    name: u32                   │  String table index (0xFFFFFFFF = anonymous)
│    arity: u16                  │
│    has_rest: u8                │  0 or 1
│    n_upvalue_descs: u16        │
│    upvalue_descs: [UpvalueDesc]│
│    n_upvalue_names: u16        │
│    upvalue_names: [u32 name]   │  Lexical names aligned with upvalue_descs
│    chunk: [Chunk data]         │  Same format as Main Chunk
│    n_local_names: u16          │
│    local_names: [(u16 slot,    │  Local variable debug info
│                   u32 name)]   │  (name = string table index)
├────────────────────────────────┤
│  Function Entry 1              │
│    ...                         │
└────────────────────────────────┘
```

### Upvalue Descriptor (3 bytes each)

| Offset | Size | Field |
|--------|------|-------|
| 0 | 1 | `kind`: 0 = ParentLocal, 1 = ParentUpvalue |
| 1 | 2 | `index`: slot/upvalue index in parent |

::: warning Bytecode inline encoding differs
The upvalue descriptors in the **function table** (above) use a compact 3-byte encoding (`u8` kind + `u16` index). However, the `MakeClosure` opcode in the **bytecode stream** uses a 4-byte encoding per upvalue: `u16` is_local + `u16` index. This wider encoding is used for alignment in the runtime bytecode.
:::

## Serialized Values (Constant Pool)

Each constant is serialized as a **type tag** (1 byte) followed by type-specific payload.

| Tag | Type | Payload |
|-----|------|---------|
| `0x00` | Nil | — (0 bytes) |
| `0x01` | Bool | 1 byte: `0x00` = false, `0x01` = true |
| `0x02` | Int | 8 bytes: i64 little-endian |
| `0x03` | Float | 8 bytes: f64 little-endian (IEEE 754) |
| `0x04` | String | 4 bytes: string table index (u32) |
| `0x05` | Symbol | 4 bytes: string table index (u32) |
| `0x06` | Keyword | 4 bytes: string table index (u32) |
| `0x07` | Char | 4 bytes: Unicode code point (u32) |
| `0x08` | List | 2 bytes: count (u16), then `count` recursive SerializedValues |
| `0x09` | Vector | 2 bytes: count (u16), then `count` recursive SerializedValues |
| `0x0A` | Map | 2 bytes: n_pairs (u16), then `n_pairs × 2` recursive SerializedValues (key, value alternating) |
| `0x0B` | HashMap | Same as Map (`0x0A`) — tag distinguishes sorted vs hash map |
| `0x0C` | Bytevector | 4 bytes: length (u32), then `length` raw bytes |

### Values That Cannot Appear in Bytecode

The following `ValueView` variants are **runtime-only** and must never appear in a `.semac` constant pool:

- `Lambda` / `Macro` — closures are constructed at runtime via `MakeClosure`
- `NativeFn` — registered by the runtime, not serializable
- `Prompt` / `Message` / `Conversation` — constructed via `__vm-prompt` / `__vm-message`
- `ToolDef` / `Agent` — constructed via `__vm-deftool` / `__vm-defagent`
- `Thunk` — created by `delay`
- `Record` — constructed by `define-record-type`
- `AsyncPromise` (tag 28) — created by `async/spawn`, runtime-only
- `Channel` (tag 29) — created by `channel/new`, runtime-only

If the serializer encounters any of these in a constant pool, it should emit a compile error.

## Spur Remapping

Sema uses `lasso::Spur` (process-local interned string handles) for symbols, keywords, and global variable names. These handles are **not stable** across processes.

### In the bytecode stream

Global variable opcodes (`LoadGlobal`, `StoreGlobal`, `DefineGlobal`, `CallGlobal`) encode Spur values as `u32`. `LoadGlobal` additionally carries a `u16` inline-cache slot operand, and `CallGlobal` carries `u16 argc` + `u16` cache slot — these are copied through unchanged; only the `u32` Spur operand is remapped. On serialization:

1. The serializer collects all Spurs referenced in the bytecode (globals, function names, local names)
2. Each Spur's string is added to the string table, getting a file-local index
3. The bytecode is **rewritten**: Spur-encoded u32 operands are replaced with string table indices

On deserialization:

1. The string table is loaded and each string is interned → new process-local Spurs
2. A remap table maps file-local indices to process-local Spurs
3. The bytecode is walked: `LoadGlobal`/`StoreGlobal`/`DefineGlobal`/`CallGlobal` operands are rewritten with the new Spur u32 values

This is the same approach Lua uses for upvalue names, and Guile uses for its symbol table.

## Source Map (Section `0x10`)

::: info Future Feature
This section is defined but not yet implemented.
:::

The source map links bytecode PCs back to source file locations, enabling error messages with file/line info when running from `.semac` files.

```
┌────────────────────────────────┐
│  source_file: u32              │  String table index of source file path
│  source_hash: [u8; 32]        │  SHA-256 of the original source
├────────────────────────────────┤
│  n_entries: u32                │
│  entries: [SourceMapEntry]     │  Sorted by PC, delta-encoded
└────────────────────────────────┘
```

### Source Map Entry (delta-encoded, variable-length)

For compact representation, entries are delta-encoded from the previous entry:

| Field | Encoding | Description |
|-------|----------|-------------|
| `delta_pc` | LEB128 u32 | PC offset from previous entry |
| `delta_line` | LEB128 i32 | Line offset from previous entry |
| `delta_col` | LEB128 i32 | Column offset from previous entry |

The first entry uses absolute values (delta from 0).

## Debug Symbols (Section `0x11`)

::: info Future Feature
This section is defined but not yet implemented.
:::

Debug symbols provide local variable names and their scope ranges within each function, enabling meaningful debugger variable inspection.

```
┌────────────────────────────────┐
│  n_functions: u32              │  Must match Function Table count
├────────────────────────────────┤
│  Function 0 debug info         │
│    n_locals: u16               │
│    locals: [LocalDebugEntry]   │
├────────────────────────────────┤
│  Function 1 debug info         │
│    ...                         │
└────────────────────────────────┘
```

### Local Debug Entry

| Offset | Size | Field |
|--------|------|-------|
| 0 | 4 | `name` — string table index |
| 4 | 2 | `slot` — local variable slot |
| 6 | 4 | `scope_start` — PC where variable comes into scope |
| 10 | 4 | `scope_end` — PC where variable goes out of scope |

## Breakpoints Section (Section `0x12`)

::: info Future Feature
This section is reserved for debugger integration. Format TBD.
:::

The breakpoints section will support:
- **Persistent breakpoints** — set breakpoints by source location; they survive recompilation
- **Conditional breakpoints** — attach Sema expressions as conditions
- **Source-mapped breakpoints** — store breakpoints as `(file, line)` pairs, resolved to PCs on load

Planned entry format:

```
┌────────────────────────────────┐
│  n_breakpoints: u32            │
├────────────────────────────────┤
│  Breakpoint Entry              │
│    source_file: u32            │  String table index
│    line: u32                   │
│    col: u32                    │  0 = any column
│    condition_len: u16          │  0 = unconditional
│    condition: [u8]             │  Sema source expression (UTF-8)
│    flags: u8                   │  0x01 = enabled, 0x02 = one-shot
└────────────────────────────────┘
```

## Debug Scopes Section (Section `0x13`)

::: info Future Feature
This section is reserved for lexical scope tracking. Format TBD.
:::

Debug scopes will map PC ranges to lexical scopes, enabling:
- Accurate "step over" / "step into" behavior
- Proper variable shadowing display in debuggers
- Scope-aware watch expressions

## Validation

When loading a `.semac` file, the loader performs these checks:

1. **Magic number** — must be `\x00SEM`
2. **Format version** — must exactly match the version this Sema build supports
3. **Reserved header field** — must be zero
4. **Section completeness** — all three required sections must be present (and string index 0 must be `""`)
5. **String table bounds** — all string table indices in the file must be in range
6. **Function table bounds** — all `func_id` references in `MakeClosure` must be valid
7. **Constant pool types** — no runtime-only value types in the constant pool
8. **Bytecode well-formedness** — opcodes must be valid, operand sizes must be correct, constant/local/upvalue indices must be in bounds, and jump targets must land on instruction boundaries

If validation fails, the loader returns a `SemaError` with a descriptive message.

::: warning Structural checks only
Validation does not verify stack discipline — a hand-crafted `.semac` with unbalanced stack operations can cause undefined behavior in the VM's unchecked hot path. Treat `.semac` files as trusted input. A stack-depth verifier is proposed (ADR #56).
:::

## Example

Given this source file:

```sema
;; hello.sema
(define greeting "Hello, World!")
(println greeting)
```

The compiled `.semac` would contain:

**String Table**: `["", "greeting", "println", "Hello, World!"]`

**Main Chunk bytecode** (conceptual):
```
0000  CONST         0    ; "Hello, World!" (string constant)
0003  DEFINE_GLOBAL 1    ; greeting (string table index → Spur)
0008  LOAD_GLOBAL   2    ; println (+ u16 inline-cache slot)
0015  LOAD_GLOBAL   1    ; greeting (+ u16 inline-cache slot)
0022  CALL          1
0025  RETURN
```

**Function Table**: (empty — no inner functions)

## Versioning Strategy

- `format_version` started at `1` and increments on any breaking change to the binary format. Version `2` added `n_global_cache_slots` and the inline-cache operands; version `3` (current) added per-function upvalue names to the debug metadata.
- `sema_major`/`sema_minor`/`sema_patch` record the compiler version for diagnostics
- The loader requires an exact `format_version` match and refuses anything else with a clear error: `"unsupported bytecode format version 1 (expected 3). Recompile from source."`
- Within the same `format_version`, new section types can be added without breaking older loaders (unknown sections are skipped)

## Comparison with Other Languages

| Feature | Sema (`.semac`) | Lua (`luac.out`) | Python (`.pyc`) | Erlang (`.beam`) | Guile (`.go`) |
|---------|-----------------|------------------|-----------------|------------------|---------------|
| Format | Flat sections | Flat binary | Header + marshal | IFF chunks | ELF container |
| Portable | No (version-tied) | No (arch-tied) | No (version-tied) | Yes | Yes |
| Debug info | Optional sections | Optional (`-s` strips) | Included | Included | Included |
| Auto-detect | Magic `\x00SEM` | Magic `\033Lua` | Magic `\xNN\r\n` | Magic `FOR1` | ELF header |
| Cache invalidation | CRC-32 source hash | N/A | Timestamp or hash | N/A | N/A |
| Spur/symbol remap | String table + rewrite | Upvalue names | marshal interning | Atom table | Symbol table |
