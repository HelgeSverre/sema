use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{
    resolve as resolve_spur, Env, EvalContext, SemaError, Spur, Value, NAN_INT_SMALL_PATTERN,
    NAN_PAYLOAD_BITS, NAN_PAYLOAD_MASK, NAN_TAG_MASK, TAG_NATIVE_FN,
};

use crate::chunk::Function;
use crate::opcodes::op;

/// A mutable cell for captured variables (upvalues).
#[derive(Debug)]
pub struct UpvalueCell {
    pub value: RefCell<Value>,
}

impl UpvalueCell {
    pub fn new(value: Value) -> Self {
        UpvalueCell {
            value: RefCell::new(value),
        }
    }
}

/// A runtime closure: function template + captured upvalues.
#[derive(Debug, Clone)]
pub struct Closure {
    pub func: Rc<Function>,
    pub upvalues: Vec<Rc<UpvalueCell>>,
}

/// Payload stored in NativeFn for VM closures.
/// Carries both the closure and the function table from its compilation context.
struct VmClosurePayload {
    closure: Rc<Closure>,
    functions: Rc<Vec<Rc<Function>>>,
}

/// A call frame in the VM's call stack.
struct CallFrame {
    closure: Rc<Closure>,
    pc: usize,
    base: usize,
    /// Open upvalue cells for locals in this frame.
    /// Maps local slot → shared UpvalueCell. Created lazily when a local is captured.
    /// `None` means no locals have been captured yet (avoids heap allocation).
    open_upvalues: Option<Vec<Option<Rc<UpvalueCell>>>>,
}

/// Number of entries in the direct-mapped global cache (must be power of 2).
const GLOBAL_CACHE_SIZE: usize = 16;

/// The bytecode virtual machine.
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: Rc<Env>,
    functions: Rc<Vec<Rc<Function>>>,
    /// Direct-mapped cache for global lookups: (spur_bits, env_version, value)
    global_cache: [(u32, u64, Value); GLOBAL_CACHE_SIZE],
}

impl VM {
    pub fn new(globals: Rc<Env>, functions: Vec<Rc<Function>>) -> Self {
        VM {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            globals,
            functions: Rc::new(functions),
            global_cache: std::array::from_fn(|_| (u32::MAX, u64::MAX, Value::nil())),
        }
    }

    fn new_with_rc_functions(globals: Rc<Env>, functions: Rc<Vec<Rc<Function>>>) -> Self {
        VM {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            globals,
            functions,
            global_cache: std::array::from_fn(|_| (u32::MAX, u64::MAX, Value::nil())),
        }
    }

    pub fn execute(&mut self, closure: Rc<Closure>, ctx: &EvalContext) -> Result<Value, SemaError> {
        let base = self.stack.len();
        // Reserve space for locals
        let n_locals = closure.func.chunk.n_locals as usize;
        self.stack.resize(base + n_locals, Value::nil());
        self.frames.push(CallFrame {
            closure,
            pc: 0,
            base,
            open_upvalues: None,
        });
        self.run(ctx)
    }

    pub fn execute_debug(
        &mut self,
        closure: Rc<Closure>,
        ctx: &EvalContext,
        debug: &mut crate::debug::DebugState,
    ) -> Result<Value, SemaError> {
        let base = self.stack.len();
        let n_locals = closure.func.chunk.n_locals as usize;
        self.stack.resize(base + n_locals, Value::nil());
        self.frames.push(CallFrame {
            closure,
            pc: 0,
            base,
            open_upvalues: None,
        });

        loop {
            match self.run_inner(ctx, Some(debug))? {
                crate::debug::VmExecResult::Finished(v) => return Ok(v),
                crate::debug::VmExecResult::Yielded => continue,
                crate::debug::VmExecResult::Stopped(info) => {
                    let _ = debug.event_tx.send(crate::debug::DebugEvent::Stopped {
                        reason: info.reason,
                        description: None,
                    });

                    loop {
                        match debug.command_rx.recv() {
                            Ok(crate::debug::DebugCommand::Continue) => {
                                debug.step_mode = crate::debug::StepMode::Continue;
                                break;
                            }
                            Ok(crate::debug::DebugCommand::StepInto) => {
                                debug.step_mode = crate::debug::StepMode::StepInto;
                                debug.step_frame_depth = self.frames.len();
                                break;
                            }
                            Ok(crate::debug::DebugCommand::StepOver) => {
                                debug.step_mode = crate::debug::StepMode::StepOver;
                                debug.step_frame_depth = self.frames.len();
                                break;
                            }
                            Ok(crate::debug::DebugCommand::StepOut) => {
                                debug.step_mode = crate::debug::StepMode::StepOut;
                                debug.step_frame_depth = self.frames.len();
                                break;
                            }
                            Ok(crate::debug::DebugCommand::Pause) => {}
                            Ok(crate::debug::DebugCommand::SetBreakpoints {
                                file,
                                lines,
                                reply,
                            }) => {
                                let ids = debug.set_breakpoints(&file, &lines);
                                let _ = reply.send(ids);
                            }
                            Ok(crate::debug::DebugCommand::GetStackTrace { reply }) => {
                                let _ = reply.send(self.debug_stack_trace());
                            }
                            Ok(crate::debug::DebugCommand::GetScopes { frame_id, reply }) => {
                                let _ = reply.send(self.debug_scopes(frame_id));
                            }
                            Ok(crate::debug::DebugCommand::GetVariables { reference, reply }) => {
                                let _ = reply.send(self.debug_variables(reference));
                            }
                            Ok(crate::debug::DebugCommand::Disconnect) => {
                                return Ok(Value::nil());
                            }
                            Err(_) => {
                                debug.step_mode = crate::debug::StepMode::Continue;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    /// Run the VM cooperatively: execute until completion or a debug stop.
    /// The caller is responsible for managing debug state between calls.
    pub fn run_cooperative(
        &mut self,
        ctx: &EvalContext,
        debug: &mut crate::debug::DebugState,
    ) -> Result<crate::debug::VmExecResult, SemaError> {
        self.run_inner(ctx, Some(debug))
    }

    /// Start cooperative debug execution: push the initial frame and run.
    pub fn start_cooperative(
        &mut self,
        closure: Rc<Closure>,
        ctx: &EvalContext,
        debug: &mut crate::debug::DebugState,
    ) -> Result<crate::debug::VmExecResult, SemaError> {
        let base = self.stack.len();
        let n_locals = closure.func.chunk.n_locals as usize;
        self.stack.resize(base + n_locals, Value::nil());
        self.frames.push(CallFrame {
            closure,
            pc: 0,
            base,
            open_upvalues: None,
        });
        self.run_inner(ctx, Some(debug))
    }

    /// Number of active call frames.
    pub fn frame_count(&self) -> usize {
        self.frames.len()
    }

    fn run(&mut self, ctx: &EvalContext) -> Result<Value, SemaError> {
        match self.run_inner(ctx, None)? {
            crate::debug::VmExecResult::Finished(v) => Ok(v),
            crate::debug::VmExecResult::Stopped(_) | crate::debug::VmExecResult::Yielded => {
                unreachable!("Stopped/Yielded without debug state")
            }
        }
    }

    fn run_inner(
        &mut self,
        ctx: &EvalContext,
        mut debug: Option<&mut crate::debug::DebugState>,
    ) -> Result<crate::debug::VmExecResult, SemaError> {
        // Raw-pointer macros for reading operands without bounds checks in inner loop
        macro_rules! read_u16 {
            ($code:expr, $pc:expr) => {{
                let v = unsafe { u16::from_le_bytes([*$code.add($pc), *$code.add($pc + 1)]) };
                $pc += 2;
                v
            }};
        }
        macro_rules! read_i32 {
            ($code:expr, $pc:expr) => {{
                let v = unsafe {
                    i32::from_le_bytes([
                        *$code.add($pc),
                        *$code.add($pc + 1),
                        *$code.add($pc + 2),
                        *$code.add($pc + 3),
                    ])
                };
                $pc += 4;
                v
            }};
        }
        macro_rules! read_u32 {
            ($code:expr, $pc:expr) => {{
                let v = unsafe {
                    u32::from_le_bytes([
                        *$code.add($pc),
                        *$code.add($pc + 1),
                        *$code.add($pc + 2),
                        *$code.add($pc + 3),
                    ])
                };
                $pc += 4;
                v
            }};
        }

        // Unsafe unchecked pop — valid when compiler guarantees stack correctness.
        #[inline(always)]
        unsafe fn pop_unchecked(stack: &mut Vec<Value>) -> Value {
            let len = stack.len();
            debug_assert!(len > 0, "pop_unchecked on empty stack");
            let v = std::ptr::read(stack.as_ptr().add(len - 1));
            stack.set_len(len - 1);
            v
        }

        // Branchless sign-extension shift for NaN-boxed small ints
        const SIGN_SHIFT: u32 = 64 - NAN_PAYLOAD_BITS;

        // Cold-path macro: saves pc, handles exception, and dispatches or returns.
        // Keeps the error path out of the hot instruction sequence.
        macro_rules! handle_err {
            ($self:expr, $fi:expr, $pc:expr, $err:expr, $saved_pc:expr, $label:tt) => {{
                $self.frames[$fi].pc = $pc;
                match $self.handle_exception($err, $saved_pc)? {
                    ExceptionAction::Handled => continue $label,
                    ExceptionAction::Propagate(e) => return Err(e),
                }
            }};
        }

        // Two-level dispatch: outer loop caches frame locals, inner loop dispatches opcodes.
        // We only break to the outer loop when frames change (Call/TailCall/Return/exceptions).
        let mut debug_poll_counter: u32 = 0;
        // Track whether we've been through at least one dispatch iteration.
        // On frame transitions (Call/TailCall/Return), resume_skip is cleared
        // so breakpoints re-trigger on new loop iterations.
        let mut dispatch_count: u32 = 0;
        'dispatch: loop {
            if dispatch_count > 0 {
                if let Some(ref mut dbg) = debug {
                    dbg.resume_skip = false;
                }
            }
            dispatch_count += 1;
            let fi = self.frames.len() - 1;
            let frame = &self.frames[fi];
            let code = frame.closure.func.chunk.code.as_ptr();
            let consts: *const [Value] = frame.closure.func.chunk.consts.as_slice();
            let base = frame.base;
            let mut pc = frame.pc;
            let has_open_upvalues = frame.open_upvalues.is_some();
            let code_len = frame.closure.func.chunk.code.len();

            // Cache the next span boundary to avoid binary_search per instruction
            let (mut next_span_idx, mut next_span_pc) = if debug.is_some() {
                let spans = &frame.closure.func.chunk.spans;
                let idx = match spans.binary_search_by_key(&(pc as u32), |(p, _)| *p) {
                    Ok(i) => i,
                    Err(i) => i,
                };
                let npc = spans.get(idx).map(|(p, _)| *p).unwrap_or(u32::MAX);
                (idx, npc)
            } else {
                (0, u32::MAX)
            };

            let _ = frame; // release borrow so we can mutate self

            loop {
                if pc >= code_len {
                    return Err(SemaError::eval(format!(
                        "VM: program counter out of bounds (pc={pc}, len={code_len})"
                    )));
                }
                let op = unsafe { *code.add(pc) };
                pc += 1;

                // Debug hook: span-cached check and command polling
                if let Some(ref mut dbg) = debug {
                    // Poll for Pause/Disconnect every 128 instructions
                    debug_poll_counter = debug_poll_counter.wrapping_add(1);
                    if debug_poll_counter & 127 == 0 {
                        while let Ok(cmd) = dbg.command_rx.try_recv() {
                            match cmd {
                                crate::debug::DebugCommand::Pause => {
                                    dbg.pause_requested = true;
                                }
                                crate::debug::DebugCommand::Disconnect => {
                                    self.frames[fi].pc = pc;
                                    return Ok(crate::debug::VmExecResult::Finished(Value::nil()));
                                }
                                crate::debug::DebugCommand::SetBreakpoints {
                                    file,
                                    lines,
                                    reply,
                                } => {
                                    let ids = dbg.set_breakpoints(&file, &lines);
                                    let _ = reply.send(ids);
                                }
                                _ => {}
                            }
                        }
                    }

                    let op_pc = (pc - 1) as u32;
                    // Fast path: skip if not at a span boundary (single integer compare)
                    let at_span = if op_pc == next_span_pc {
                        let spans = &self.frames[fi].closure.func.chunk.spans;
                        let line = spans[next_span_idx].1.line as u32;
                        let file = self.frames[fi].closure.func.source_file.clone();
                        next_span_idx += 1;
                        next_span_pc = spans
                            .get(next_span_idx)
                            .map(|(p, _)| *p)
                            .unwrap_or(u32::MAX);
                        Some((file, line))
                    } else if op_pc > next_span_pc {
                        // Jumped past — resync via binary search
                        let spans = &self.frames[fi].closure.func.chunk.spans;
                        match spans.binary_search_by_key(&op_pc, |(p, _)| *p) {
                            Ok(i) => {
                                let line = spans[i].1.line as u32;
                                let file = self.frames[fi].closure.func.source_file.clone();
                                next_span_idx = i + 1;
                                next_span_pc = spans
                                    .get(next_span_idx)
                                    .map(|(p, _)| *p)
                                    .unwrap_or(u32::MAX);
                                Some((file, line))
                            }
                            Err(i) => {
                                next_span_idx = i;
                                next_span_pc = spans.get(i).map(|(p, _)| *p).unwrap_or(u32::MAX);
                                None
                            }
                        }
                    } else if next_span_idx > 0 {
                        // Check for backward jump: op_pc is before our current
                        // span window (e.g., loop back-edge). Resync via binary search.
                        // Clear resume_skip so breakpoints re-trigger on new iterations.
                        let spans = &self.frames[fi].closure.func.chunk.spans;
                        if op_pc < spans[next_span_idx - 1].0 {
                            dbg.resume_skip = false;
                            match spans.binary_search_by_key(&op_pc, |(p, _)| *p) {
                                Ok(i) => {
                                    let line = spans[i].1.line as u32;
                                    let file = self.frames[fi].closure.func.source_file.clone();
                                    next_span_idx = i + 1;
                                    next_span_pc = spans
                                        .get(next_span_idx)
                                        .map(|(p, _)| *p)
                                        .unwrap_or(u32::MAX);
                                    Some((file, line))
                                }
                                Err(i) => {
                                    next_span_idx = i;
                                    next_span_pc =
                                        spans.get(i).map(|(p, _)| *p).unwrap_or(u32::MAX);
                                    None
                                }
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if let Some((file, line)) = at_span {
                        if dbg.resume_skip {
                            // Keep skipping while on the same line as last stop.
                            // This prevents re-triggering breakpoints on multi-opcode lines.
                            let same_line = dbg
                                .last_stop_line
                                .as_ref()
                                .is_some_and(|(_, last_line)| line == *last_line);
                            if !same_line {
                                dbg.resume_skip = false;
                            }
                        }
                        if !dbg.resume_skip {
                            let frame_depth = self.frames.len();
                            if dbg.should_stop(file.as_ref(), line, frame_depth) {
                                self.frames[fi].pc = pc - 1;
                                let reason = if dbg.pause_requested {
                                    crate::debug::StopReason::Pause
                                } else if dbg.step_mode != crate::debug::StepMode::Continue {
                                    crate::debug::StopReason::Step
                                } else {
                                    crate::debug::StopReason::Breakpoint
                                };
                                dbg.last_stop_line = file.as_ref().map(|f| (f.clone(), line));
                                dbg.pause_requested = false;
                                dbg.resume_skip = true;
                                return Ok(crate::debug::VmExecResult::Stopped(
                                    crate::debug::StopInfo {
                                        reason,
                                        file: file.clone(),
                                        line,
                                    },
                                ));
                            }
                        }
                    }

                    // Instruction budget yield check (for cooperative WASM execution).
                    // Checked every 128 instructions, after breakpoints so they take priority.
                    if debug_poll_counter & 127 == 0 && dbg.instructions_remaining > 0 {
                        dbg.instructions_remaining = dbg.instructions_remaining.saturating_sub(128);
                        if dbg.instructions_remaining == 0 {
                            self.frames[fi].pc = pc - 1;
                            return Ok(crate::debug::VmExecResult::Yielded);
                        }
                    }
                }

                match op {
                    // --- Constants & stack ---
                    op::CONST => {
                        let idx = read_u16!(code, pc) as usize;
                        let val = unsafe { (&(*consts)).get_unchecked(idx) }.clone();
                        self.stack.push(val);
                    }
                    op::NIL => {
                        self.stack.push(Value::nil());
                    }
                    op::TRUE => {
                        self.stack.push(Value::bool(true));
                    }
                    op::FALSE => {
                        self.stack.push(Value::bool(false));
                    }
                    op::POP => {
                        unsafe { pop_unchecked(&mut self.stack) };
                    }
                    op::DUP => {
                        let val =
                            unsafe { &*self.stack.as_ptr().add(self.stack.len() - 1) }.clone();
                        self.stack.push(val);
                    }

                    // --- Locals ---
                    op::LOAD_LOCAL => {
                        let slot = read_u16!(code, pc) as usize;
                        let val = if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(slot) {
                                    cell.value.borrow().clone()
                                } else {
                                    self.stack[base + slot].clone()
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            self.stack[base + slot].clone()
                        };
                        self.stack.push(val);
                    }
                    op::STORE_LOCAL => {
                        let slot = read_u16!(code, pc) as usize;
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack[base + slot] = val.clone();
                        if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(slot) {
                                    *cell.value.borrow_mut() = val;
                                }
                            }
                        }
                    }

                    // --- Upvalues ---
                    op::LOAD_UPVALUE => {
                        let idx = read_u16!(code, pc) as usize;
                        let val = self.frames[fi].closure.upvalues[idx].value.borrow().clone();
                        self.stack.push(val);
                    }
                    op::STORE_UPVALUE => {
                        let idx = read_u16!(code, pc) as usize;
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        *self.frames[fi].closure.upvalues[idx].value.borrow_mut() = val;
                    }

                    // --- Globals ---
                    op::LOAD_GLOBAL => {
                        let bits = read_u32!(code, pc);
                        let version = self.globals.version.get();
                        let slot = (bits as usize) & (GLOBAL_CACHE_SIZE - 1);
                        let entry = &self.global_cache[slot];
                        if entry.0 == bits && entry.1 == version {
                            self.stack.push(entry.2.clone());
                        } else {
                            let spur: Spur = unsafe { std::mem::transmute::<u32, Spur>(bits) };
                            match self.globals.get(spur) {
                                Some(val) => {
                                    self.global_cache[slot] = (bits, version, val.clone());
                                    self.stack.push(val);
                                }
                                None => {
                                    let err = SemaError::Unbound(resolve_spur(spur));
                                    handle_err!(self, fi, pc, err, pc - op::SIZE_OP_U32, 'dispatch);
                                }
                            }
                        }
                    }
                    op::STORE_GLOBAL => {
                        let bits = read_u32!(code, pc);
                        let spur: Spur = unsafe { std::mem::transmute::<u32, Spur>(bits) };
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        if !self.globals.set_existing(spur, val.clone()) {
                            self.globals.set(spur, val);
                        }
                    }
                    op::DEFINE_GLOBAL => {
                        let bits = read_u32!(code, pc);
                        let spur: Spur = unsafe { std::mem::transmute::<u32, Spur>(bits) };
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.globals.set(spur, val);
                    }

                    // --- Control flow ---
                    op::JUMP => {
                        let offset = read_i32!(code, pc);
                        pc = (pc as i64 + offset as i64) as usize;
                    }
                    op::JUMP_IF_FALSE => {
                        let offset = read_i32!(code, pc);
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        if !val.is_truthy() {
                            pc = (pc as i64 + offset as i64) as usize;
                        }
                    }
                    op::JUMP_IF_TRUE => {
                        let offset = read_i32!(code, pc);
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        if val.is_truthy() {
                            pc = (pc as i64 + offset as i64) as usize;
                        }
                    }

                    // --- Function calls ---
                    op::CALL => {
                        let argc = read_u16!(code, pc) as usize;
                        self.frames[fi].pc = pc;
                        let saved_pc = pc - op::SIZE_OP_U16;
                        if let Err(err) = self.call_value(argc, ctx) {
                            match self.handle_exception(err, saved_pc)? {
                                ExceptionAction::Handled => {}
                                ExceptionAction::Propagate(e) => return Err(e),
                            }
                        }
                        continue 'dispatch;
                    }
                    op::TAIL_CALL => {
                        let argc = read_u16!(code, pc) as usize;
                        self.frames[fi].pc = pc;
                        let saved_pc = pc - op::SIZE_OP_U16;
                        if let Err(err) = self.tail_call_value(argc, ctx) {
                            match self.handle_exception(err, saved_pc)? {
                                ExceptionAction::Handled => {}
                                ExceptionAction::Propagate(e) => return Err(e),
                            }
                        }
                        continue 'dispatch;
                    }
                    op::RETURN => {
                        let result = if !self.stack.is_empty() {
                            unsafe { pop_unchecked(&mut self.stack) }
                        } else {
                            Value::nil()
                        };
                        let frame = self.frames.pop().unwrap();
                        self.stack.truncate(frame.base);
                        if self.frames.is_empty() {
                            return Ok(crate::debug::VmExecResult::Finished(result));
                        }
                        self.stack.push(result);
                        continue 'dispatch;
                    }

                    // --- Closures ---
                    op::MAKE_CLOSURE => {
                        self.frames[fi].pc = pc - op::SIZE_OP; // make_closure reads from frame.pc (the opcode position)
                        self.make_closure()?;
                        continue 'dispatch;
                    }

                    op::CALL_NATIVE => {
                        let _native_id = read_u16!(code, pc);
                        let _argc = read_u16!(code, pc);
                        self.frames[fi].pc = pc;
                        return Err(SemaError::eval("VM: CallNative not yet implemented"));
                    }

                    // --- Data constructors ---
                    op::MAKE_LIST => {
                        let n = read_u16!(code, pc) as usize;
                        let start = self.stack.len() - n;
                        let items: Vec<Value> = self.stack.drain(start..).collect();
                        self.stack.push(Value::list(items));
                    }
                    op::MAKE_VECTOR => {
                        let n = read_u16!(code, pc) as usize;
                        let start = self.stack.len() - n;
                        let items: Vec<Value> = self.stack.drain(start..).collect();
                        self.stack.push(Value::vector(items));
                    }
                    op::MAKE_MAP => {
                        let n = read_u16!(code, pc) as usize;
                        let start = self.stack.len() - n * 2;
                        let items: Vec<Value> = self.stack.drain(start..).collect();
                        let mut map = BTreeMap::new();
                        for pair in items.chunks(2) {
                            map.insert(pair[0].clone(), pair[1].clone());
                        }
                        self.stack.push(Value::map(map));
                    }
                    op::MAKE_HASH_MAP => {
                        let n = read_u16!(code, pc) as usize;
                        let start = self.stack.len() - n * 2;
                        let items: Vec<Value> = self.stack.drain(start..).collect();
                        let mut map = hashbrown::HashMap::new();
                        for pair in items.chunks(2) {
                            map.insert(pair[0].clone(), pair[1].clone());
                        }
                        self.stack.push(Value::hashmap_from_rc(Rc::new(map)));
                    }

                    // --- Exceptions ---
                    op::THROW => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        let err = SemaError::UserException(val);
                        handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                    }

                    // --- Arithmetic ---
                    op::ADD => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_add(&a, &b) {
                            Ok(v) => self.stack.push(v),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }
                    op::SUB => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_sub(&a, &b) {
                            Ok(v) => self.stack.push(v),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }
                    op::MUL => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_mul(&a, &b) {
                            Ok(v) => self.stack.push(v),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }
                    op::DIV => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_div(&a, &b) {
                            Ok(v) => self.stack.push(v),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }
                    op::NEGATE => {
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        if let Some(n) = a.as_int() {
                            self.stack.push(Value::int(-n));
                        } else if let Some(f) = a.as_float() {
                            self.stack.push(Value::float(-f));
                        } else {
                            let err = SemaError::type_error("number", a.type_name());
                            handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                        }
                    }
                    op::NOT => {
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack.push(Value::bool(!a.is_truthy()));
                    }
                    op::EQ => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack.push(Value::bool(vm_eq(&a, &b)));
                    }
                    op::LT => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_lt(&a, &b) {
                            Ok(v) => self.stack.push(Value::bool(v)),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }
                    op::GT => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_lt(&b, &a) {
                            Ok(v) => self.stack.push(Value::bool(v)),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }
                    op::LE => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_lt(&b, &a) {
                            Ok(v) => self.stack.push(Value::bool(!v)),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }
                    op::GE => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        match vm_lt(&a, &b) {
                            Ok(v) => self.stack.push(Value::bool(!v)),
                            Err(err) => handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch),
                        }
                    }

                    // --- Specialized int fast paths ---
                    // These operate directly on raw u64 bits to avoid Clone/Drop overhead.
                    // Small ints are immediates (no heap pointer), so we can safely
                    // overwrite stack slots and adjust length without running destructors.
                    op::ADD_INT => {
                        let len = self.stack.len();
                        let a_bits = unsafe { (*self.stack.as_ptr().add(len - 2)).raw_bits() };
                        let b_bits = unsafe { (*self.stack.as_ptr().add(len - 1)).raw_bits() };
                        if (a_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                            && (b_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                        {
                            let sum = (a_bits.wrapping_add(b_bits)) & NAN_PAYLOAD_MASK;
                            let result = NAN_INT_SMALL_PATTERN | sum;
                            unsafe {
                                std::ptr::write(
                                    self.stack.as_mut_ptr().add(len - 2),
                                    Value::from_raw_bits(result),
                                );
                                self.stack.set_len(len - 1);
                            }
                        } else {
                            let b = unsafe { pop_unchecked(&mut self.stack) };
                            let a = unsafe { pop_unchecked(&mut self.stack) };
                            match vm_add(&a, &b) {
                                Ok(v) => self.stack.push(v),
                                Err(err) => {
                                    handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch)
                                }
                            }
                        }
                    }
                    op::SUB_INT => {
                        let len = self.stack.len();
                        let a_bits = unsafe { (*self.stack.as_ptr().add(len - 2)).raw_bits() };
                        let b_bits = unsafe { (*self.stack.as_ptr().add(len - 1)).raw_bits() };
                        if (a_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                            && (b_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                        {
                            let diff = (a_bits.wrapping_sub(b_bits)) & NAN_PAYLOAD_MASK;
                            let result = NAN_INT_SMALL_PATTERN | diff;
                            unsafe {
                                std::ptr::write(
                                    self.stack.as_mut_ptr().add(len - 2),
                                    Value::from_raw_bits(result),
                                );
                                self.stack.set_len(len - 1);
                            }
                        } else {
                            let b = unsafe { pop_unchecked(&mut self.stack) };
                            let a = unsafe { pop_unchecked(&mut self.stack) };
                            match vm_sub(&a, &b) {
                                Ok(v) => self.stack.push(v),
                                Err(err) => {
                                    handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch)
                                }
                            }
                        }
                    }
                    op::MUL_INT => {
                        let len = self.stack.len();
                        let a_bits = unsafe { (*self.stack.as_ptr().add(len - 2)).raw_bits() };
                        let b_bits = unsafe { (*self.stack.as_ptr().add(len - 1)).raw_bits() };
                        if (a_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                            && (b_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                        {
                            // Branchless sign-extension to i64
                            let ax =
                                (((a_bits & NAN_PAYLOAD_MASK) << SIGN_SHIFT) as i64) >> SIGN_SHIFT;
                            let bx =
                                (((b_bits & NAN_PAYLOAD_MASK) << SIGN_SHIFT) as i64) >> SIGN_SHIFT;
                            // Use Value::int for multiplication — result may overflow 45 bits
                            unsafe {
                                std::ptr::write(
                                    self.stack.as_mut_ptr().add(len - 2),
                                    Value::int(ax.wrapping_mul(bx)),
                                );
                                self.stack.set_len(len - 1);
                            }
                        } else {
                            let b = unsafe { pop_unchecked(&mut self.stack) };
                            let a = unsafe { pop_unchecked(&mut self.stack) };
                            match vm_mul(&a, &b) {
                                Ok(v) => self.stack.push(v),
                                Err(err) => {
                                    handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch)
                                }
                            }
                        }
                    }
                    op::LT_INT => {
                        let len = self.stack.len();
                        let a_bits = unsafe { (*self.stack.as_ptr().add(len - 2)).raw_bits() };
                        let b_bits = unsafe { (*self.stack.as_ptr().add(len - 1)).raw_bits() };
                        if (a_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                            && (b_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                        {
                            // Branchless sign-extension and compare
                            let ax =
                                (((a_bits & NAN_PAYLOAD_MASK) << SIGN_SHIFT) as i64) >> SIGN_SHIFT;
                            let bx =
                                (((b_bits & NAN_PAYLOAD_MASK) << SIGN_SHIFT) as i64) >> SIGN_SHIFT;
                            unsafe {
                                std::ptr::write(
                                    self.stack.as_mut_ptr().add(len - 2),
                                    Value::bool(ax < bx),
                                );
                                self.stack.set_len(len - 1);
                            }
                        } else {
                            let b = unsafe { pop_unchecked(&mut self.stack) };
                            let a = unsafe { pop_unchecked(&mut self.stack) };
                            match vm_lt(&a, &b) {
                                Ok(v) => self.stack.push(Value::bool(v)),
                                Err(err) => {
                                    handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch)
                                }
                            }
                        }
                    }
                    op::EQ_INT => {
                        let len = self.stack.len();
                        let a_bits = unsafe { (*self.stack.as_ptr().add(len - 2)).raw_bits() };
                        let b_bits = unsafe { (*self.stack.as_ptr().add(len - 1)).raw_bits() };
                        if (a_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                            && (b_bits & NAN_TAG_MASK) == NAN_INT_SMALL_PATTERN
                        {
                            // Small ints: equal iff same bits
                            unsafe {
                                std::ptr::write(
                                    self.stack.as_mut_ptr().add(len - 2),
                                    Value::bool(a_bits == b_bits),
                                );
                                self.stack.set_len(len - 1);
                            }
                        } else {
                            let b = unsafe { pop_unchecked(&mut self.stack) };
                            let a = unsafe { pop_unchecked(&mut self.stack) };
                            self.stack.push(Value::bool(vm_eq(&a, &b)));
                        }
                    }

                    op::LOAD_LOCAL0 => {
                        let val = if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.first() {
                                    cell.value.borrow().clone()
                                } else {
                                    self.stack[base].clone()
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            self.stack[base].clone()
                        };
                        self.stack.push(val);
                    }
                    op::LOAD_LOCAL1 => {
                        let val = if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(1) {
                                    cell.value.borrow().clone()
                                } else {
                                    self.stack[base + 1].clone()
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            self.stack[base + 1].clone()
                        };
                        self.stack.push(val);
                    }
                    op::LOAD_LOCAL2 => {
                        let val = if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(2) {
                                    cell.value.borrow().clone()
                                } else {
                                    self.stack[base + 2].clone()
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            self.stack[base + 2].clone()
                        };
                        self.stack.push(val);
                    }
                    op::LOAD_LOCAL3 => {
                        let val = if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(3) {
                                    cell.value.borrow().clone()
                                } else {
                                    self.stack[base + 3].clone()
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            self.stack[base + 3].clone()
                        };
                        self.stack.push(val);
                    }

                    // Fused LOAD_GLOBAL + CALL: look up global, call without
                    // pushing the function value onto the stack.
                    op::CALL_GLOBAL => {
                        let bits = read_u32!(code, pc);
                        let argc = read_u16!(code, pc) as usize;
                        self.frames[fi].pc = pc;
                        let saved_pc = pc - op::SIZE_CALL_GLOBAL;

                        // Look up the global (with cache)
                        let version = self.globals.version.get();
                        let slot = (bits as usize) & (GLOBAL_CACHE_SIZE - 1);
                        let entry = &self.global_cache[slot];
                        let func_val = if entry.0 == bits && entry.1 == version {
                            entry.2.clone()
                        } else {
                            let spur: Spur = unsafe { std::mem::transmute::<u32, Spur>(bits) };
                            match self.globals.get(spur) {
                                Some(val) => {
                                    self.global_cache[slot] = (bits, version, val.clone());
                                    val
                                }
                                None => {
                                    let err = SemaError::Unbound(resolve_spur(spur));
                                    handle_err!(self, fi, pc, err, saved_pc, 'dispatch);
                                }
                            }
                        };

                        // Fast path: VM closure — use direct call without function slot
                        if func_val.raw_tag() == Some(TAG_NATIVE_FN) {
                            let vm_closure_data = {
                                let native = func_val.as_native_fn_ref().unwrap();
                                native.payload.as_ref().and_then(|p| {
                                    p.downcast_ref::<VmClosurePayload>().map(|vmc| {
                                        let closure = vmc.closure.clone();
                                        let functions =
                                            if Rc::ptr_eq(&vmc.functions, &self.functions) {
                                                None
                                            } else {
                                                Some(vmc.functions.clone())
                                            };
                                        (closure, functions)
                                    })
                                })
                            };
                            if let Some((closure, functions)) = vm_closure_data {
                                if let Some(f) = functions {
                                    self.functions = f;
                                }
                                if let Err(err) = self.call_vm_closure_direct(closure, argc) {
                                    match self.handle_exception(err, saved_pc)? {
                                        ExceptionAction::Handled => {}
                                        ExceptionAction::Propagate(e) => return Err(e),
                                    }
                                }
                                continue 'dispatch;
                            }
                        }

                        // Slow path: non-VM callable — use call_value_with
                        if let Err(err) = self.call_value_with(func_val, argc, ctx) {
                            match self.handle_exception(err, saved_pc)? {
                                ExceptionAction::Handled => {}
                                ExceptionAction::Propagate(e) => return Err(e),
                            }
                        }
                        continue 'dispatch;
                    }

                    op::STORE_LOCAL0 => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack[base] = val.clone();
                        if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.first() {
                                    *cell.value.borrow_mut() = val;
                                }
                            }
                        }
                    }
                    op::STORE_LOCAL1 => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack[base + 1] = val.clone();
                        if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(1) {
                                    *cell.value.borrow_mut() = val;
                                }
                            }
                        }
                    }
                    op::STORE_LOCAL2 => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack[base + 2] = val.clone();
                        if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(2) {
                                    *cell.value.borrow_mut() = val;
                                }
                            }
                        }
                    }
                    op::STORE_LOCAL3 => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack[base + 3] = val.clone();
                        if has_open_upvalues {
                            if let Some(ref open) = self.frames[fi].open_upvalues {
                                if let Some(Some(cell)) = open.get(3) {
                                    *cell.value.borrow_mut() = val;
                                }
                            }
                        }
                    }

                    // --- Inline stdlib intrinsics ---
                    op::CAR => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        if let Some(l) = val.as_list() {
                            self.stack.push(if l.is_empty() {
                                Value::nil()
                            } else {
                                l[0].clone()
                            });
                        } else if let Some(v) = val.as_vector() {
                            self.stack.push(if v.is_empty() {
                                Value::nil()
                            } else {
                                v[0].clone()
                            });
                        } else {
                            let err = SemaError::type_error("list or vector", val.type_name());
                            handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                        }
                    }
                    op::CDR => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        if let Some(l) = val.as_list() {
                            self.stack.push(if l.len() <= 1 {
                                Value::list(vec![])
                            } else {
                                Value::list(l[1..].to_vec())
                            });
                        } else if let Some(v) = val.as_vector() {
                            self.stack.push(if v.len() <= 1 {
                                Value::vector(vec![])
                            } else {
                                Value::vector(v[1..].to_vec())
                            });
                        } else {
                            let err = SemaError::type_error("list or vector", val.type_name());
                            handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                        }
                    }
                    op::CONS => {
                        let tail = unsafe { pop_unchecked(&mut self.stack) };
                        let head = unsafe { pop_unchecked(&mut self.stack) };
                        if tail.is_nil() {
                            self.stack.push(Value::list(vec![head]));
                        } else if let Some(list) = tail.as_list() {
                            let mut new = Vec::with_capacity(1 + list.len());
                            new.push(head);
                            new.extend(list.iter().cloned());
                            self.stack.push(Value::list(new));
                        } else {
                            self.stack.push(Value::list(vec![head, tail]));
                        }
                    }
                    op::IS_NULL => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        let result = val.is_nil() || val.as_list().is_some_and(|l| l.is_empty());
                        self.stack.push(Value::bool(result));
                    }
                    op::IS_PAIR => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        let result = val.as_list().is_some_and(|l| !l.is_empty());
                        self.stack.push(Value::bool(result));
                    }
                    op::IS_LIST => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack.push(Value::bool(val.is_list()));
                    }
                    op::IS_NUMBER => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack.push(Value::bool(val.is_int() || val.is_float()));
                    }
                    op::IS_STRING => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack.push(Value::bool(val.is_string()));
                    }
                    op::IS_SYMBOL => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        self.stack.push(Value::bool(val.is_symbol()));
                    }
                    op::LENGTH => {
                        let val = unsafe { pop_unchecked(&mut self.stack) };
                        if let Some(l) = val.as_list() {
                            self.stack.push(Value::int(l.len() as i64));
                        } else if let Some(v) = val.as_vector() {
                            self.stack.push(Value::int(v.len() as i64));
                        } else if let Some(s) = val.as_str() {
                            self.stack.push(Value::int(s.chars().count() as i64));
                        } else if let Some(m) = val.as_map_rc() {
                            self.stack.push(Value::int(m.len() as i64));
                        } else if let Some(m) = val.as_hashmap_rc() {
                            self.stack.push(Value::int(m.len() as i64));
                        } else {
                            let err = SemaError::type_error(
                                "list, vector, string, map, or hashmap",
                                val.type_name(),
                            );
                            handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                        }
                    }

                    op::APPEND => {
                        let b = unsafe { pop_unchecked(&mut self.stack) };
                        let a = unsafe { pop_unchecked(&mut self.stack) };
                        if let (Some(la), Some(lb)) = (a.as_list(), b.as_list()) {
                            let mut result = Vec::with_capacity(la.len() + lb.len());
                            result.extend(la.iter().cloned());
                            result.extend(lb.iter().cloned());
                            self.stack.push(Value::list(result));
                        } else {
                            let mut result = Vec::new();
                            if let Some(l) = a.as_list() {
                                result.extend(l.iter().cloned());
                            } else if let Some(v) = a.as_vector() {
                                result.extend(v.iter().cloned());
                            } else {
                                let err = SemaError::type_error("list or vector", a.type_name());
                                handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                            }
                            if let Some(l) = b.as_list() {
                                result.extend(l.iter().cloned());
                            } else if let Some(v) = b.as_vector() {
                                result.extend(v.iter().cloned());
                            } else {
                                let err = SemaError::type_error("list or vector", b.type_name());
                                handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                            }
                            self.stack.push(Value::list(result));
                        }
                    }
                    op::GET => {
                        let key = unsafe { pop_unchecked(&mut self.stack) };
                        let coll = unsafe { pop_unchecked(&mut self.stack) };
                        if let Some(map) = coll.as_hashmap_ref() {
                            self.stack
                                .push(map.get(&key).cloned().unwrap_or(Value::nil()));
                        } else if let Some(map) = coll.as_map_ref() {
                            self.stack
                                .push(map.get(&key).cloned().unwrap_or(Value::nil()));
                        } else {
                            let err = SemaError::type_error("map or hashmap", coll.type_name());
                            handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                        }
                    }
                    op::CONTAINS_Q => {
                        let key = unsafe { pop_unchecked(&mut self.stack) };
                        let coll = unsafe { pop_unchecked(&mut self.stack) };
                        if let Some(map) = coll.as_hashmap_ref() {
                            self.stack.push(Value::bool(map.contains_key(&key)));
                        } else if let Some(map) = coll.as_map_ref() {
                            self.stack.push(Value::bool(map.contains_key(&key)));
                        } else {
                            let err = SemaError::type_error("map or hashmap", coll.type_name());
                            handle_err!(self, fi, pc, err, pc - op::SIZE_OP, 'dispatch);
                        }
                    }

                    _ => {
                        return Err(SemaError::eval(format!("VM: invalid opcode {}", op)));
                    }
                }
            }
        }
    }

    // --- Function call implementation ---

    fn call_value(&mut self, argc: usize, ctx: &EvalContext) -> Result<(), SemaError> {
        let func_idx = self.stack.len() - 1 - argc;

        // Fast path: peek at tag without Rc refcount bump.
        if self.stack[func_idx].raw_tag() == Some(TAG_NATIVE_FN) {
            // Check for VM closure payload without holding a borrow across mutation.
            // Extract closure (cloned Rc) in a block; only clone functions if different.
            let vm_closure_data = {
                let native = self.stack[func_idx].as_native_fn_ref().unwrap();
                native.payload.as_ref().and_then(|p| {
                    p.downcast_ref::<VmClosurePayload>().map(|vmc| {
                        let closure = vmc.closure.clone();
                        // Only clone functions Rc if it's a different table
                        let functions = if Rc::ptr_eq(&vmc.functions, &self.functions) {
                            None
                        } else {
                            Some(vmc.functions.clone())
                        };
                        (closure, functions)
                    })
                })
            };
            if let Some((closure, functions)) = vm_closure_data {
                if let Some(f) = functions {
                    self.functions = f;
                }
                return self.call_vm_closure(closure, argc);
            }
            // Regular native fn — need Rc for the call
            let func_rc = self.stack[func_idx].as_native_fn_rc().unwrap();
            let args_start = func_idx + 1;
            let args: Vec<Value> = self.stack.drain(args_start..).collect();
            self.stack.truncate(func_idx);
            let result = (func_rc.func)(ctx, &args)?;
            self.stack.push(result);
            Ok(())
        } else if let Some(kw) = self.stack[func_idx].as_keyword_spur() {
            // Keyword as function: (kw map) -> map[kw]
            if argc != 1 {
                return Err(SemaError::arity(resolve_spur(kw), "1", argc));
            }
            let arg = self.stack.pop().unwrap();
            self.stack.pop(); // pop keyword
            let kw_val = Value::keyword_from_spur(kw);
            let result = if let Some(m) = arg.as_map_rc() {
                m.get(&kw_val).cloned().unwrap_or(Value::nil())
            } else if let Some(m) = arg.as_hashmap_rc() {
                m.get(&kw_val).cloned().unwrap_or(Value::nil())
            } else {
                return Err(SemaError::type_error("map or hashmap", arg.type_name()));
            };
            self.stack.push(result);
            Ok(())
        } else {
            // Lambda or other callable — clone once and use call_callback
            let func_val = self.stack[func_idx].clone();
            let args_start = func_idx + 1;
            let args: Vec<Value> = self.stack.drain(args_start..).collect();
            self.stack.truncate(func_idx);
            let result = sema_core::call_callback(ctx, &func_val, &args)?;
            self.stack.push(result);
            Ok(())
        }
    }

    fn tail_call_value(&mut self, argc: usize, ctx: &EvalContext) -> Result<(), SemaError> {
        let func_idx = self.stack.len() - 1 - argc;

        // Fast path: peek at tag without Rc refcount bump
        if self.stack[func_idx].raw_tag() == Some(TAG_NATIVE_FN) {
            let vm_closure_data = {
                let native = self.stack[func_idx].as_native_fn_ref().unwrap();
                native.payload.as_ref().and_then(|p| {
                    p.downcast_ref::<VmClosurePayload>().map(|vmc| {
                        let closure = vmc.closure.clone();
                        let functions = if Rc::ptr_eq(&vmc.functions, &self.functions) {
                            None
                        } else {
                            Some(vmc.functions.clone())
                        };
                        (closure, functions)
                    })
                })
            };
            if let Some((closure, functions)) = vm_closure_data {
                if let Some(f) = functions {
                    self.functions = f;
                }
                return self.tail_call_vm_closure(closure, argc);
            }
        }

        // Non-VM callables: regular call (no TCO possible)
        self.call_value(argc, ctx)
    }

    /// Call a function value that's NOT on the stack (for CALL_GLOBAL slow path).
    /// The args are on top of the stack.
    fn call_value_with(
        &mut self,
        func_val: Value,
        argc: usize,
        ctx: &EvalContext,
    ) -> Result<(), SemaError> {
        if func_val.raw_tag() == Some(TAG_NATIVE_FN) {
            let func_rc = func_val.as_native_fn_rc().unwrap();
            let args_start = self.stack.len() - argc;
            let args: Vec<Value> = self.stack.drain(args_start..).collect();
            let result = (func_rc.func)(ctx, &args)?;
            self.stack.push(result);
            Ok(())
        } else if let Some(kw) = func_val.as_keyword_spur() {
            if argc != 1 {
                return Err(SemaError::arity(resolve_spur(kw), "1", argc));
            }
            let arg = self.stack.pop().unwrap();
            let kw_val = Value::keyword_from_spur(kw);
            let result = if let Some(m) = arg.as_map_rc() {
                m.get(&kw_val).cloned().unwrap_or(Value::nil())
            } else if let Some(m) = arg.as_hashmap_rc() {
                m.get(&kw_val).cloned().unwrap_or(Value::nil())
            } else {
                return Err(SemaError::type_error("map or hashmap", arg.type_name()));
            };
            self.stack.push(result);
            Ok(())
        } else {
            let args_start = self.stack.len() - argc;
            let args: Vec<Value> = self.stack.drain(args_start..).collect();
            let result = sema_core::call_callback(ctx, &func_val, &args)?;
            self.stack.push(result);
            Ok(())
        }
    }

    /// Push a new CallFrame for a VM closure called via CALL_GLOBAL.
    /// No function value is on the stack — only args. `base = stack.len() - argc`.
    /// Args are already in place; we just extend the stack for remaining locals.
    fn call_vm_closure_direct(
        &mut self,
        closure: Rc<Closure>,
        argc: usize,
    ) -> Result<(), SemaError> {
        let func = &closure.func;
        let arity = func.arity as usize;
        let has_rest = func.has_rest;
        let n_locals = func.chunk.n_locals as usize;

        // Arity check
        if has_rest {
            if argc < arity {
                return Err(SemaError::arity(
                    func.name
                        .map(resolve_spur)
                        .unwrap_or_else(|| "<lambda>".to_string()),
                    format!("{}+", arity),
                    argc,
                ));
            }
        } else if argc != arity {
            return Err(SemaError::arity(
                func.name
                    .map(resolve_spur)
                    .unwrap_or_else(|| "<lambda>".to_string()),
                arity.to_string(),
                argc,
            ));
        }

        // Args are already at stack[base..base+argc] in the right order.
        // base = stack.len() - argc
        let base = self.stack.len() - argc;

        if has_rest {
            // Collect extra args into a rest list
            let rest: Vec<Value> = self.stack[base + arity..base + argc].to_vec();
            self.stack.truncate(base + arity);
            self.stack.push(Value::list(rest));
        }

        // Resize to exact local count (pads with nil or truncates)
        self.stack.resize(base + n_locals, Value::nil());

        self.frames.push(CallFrame {
            closure,
            pc: 0,
            base,
            open_upvalues: None,
        });

        Ok(())
    }

    /// Push a new CallFrame for a VM closure (no Rust recursion).
    /// Caller must set `self.functions` before calling this.
    /// Takes ownership of the Rc to avoid an extra clone.
    fn call_vm_closure(&mut self, closure: Rc<Closure>, argc: usize) -> Result<(), SemaError> {
        let func = &closure.func;
        let arity = func.arity as usize;
        let has_rest = func.has_rest;
        let n_locals = func.chunk.n_locals as usize;

        // Arity check
        if has_rest {
            if argc < arity {
                return Err(SemaError::arity(
                    func.name
                        .map(resolve_spur)
                        .unwrap_or_else(|| "<lambda>".to_string()),
                    format!("{}+", arity),
                    argc,
                ));
            }
        } else if argc != arity {
            return Err(SemaError::arity(
                func.name
                    .map(resolve_spur)
                    .unwrap_or_else(|| "<lambda>".to_string()),
                arity.to_string(),
                argc,
            ));
        }

        // Copy args directly from stack into new locals — no Vec allocation
        let func_idx = self.stack.len() - 1 - argc;
        let base = func_idx; // reuse the callee's slot as new frame base

        // Copy params: clone each arg into its local slot.
        // dest (base+i) < src (func_idx+1+i) so forward copy is safe.
        Self::copy_args_to_locals(&mut self.stack, base, func_idx + 1, arity, argc, has_rest);

        // Now resize to exact local count (pads with nil or truncates excess args)
        self.stack.resize(base + n_locals, Value::nil());

        // Push frame
        self.frames.push(CallFrame {
            closure,
            pc: 0,
            base,
            open_upvalues: None,
        });

        Ok(())
    }

    /// Tail-call a VM closure: reuse the current frame's stack space.
    /// Caller must set `self.functions` before calling this.
    /// Takes ownership of the Rc to avoid an extra clone.
    fn tail_call_vm_closure(&mut self, closure: Rc<Closure>, argc: usize) -> Result<(), SemaError> {
        let func = &closure.func;
        let arity = func.arity as usize;
        let has_rest = func.has_rest;
        let n_locals = func.chunk.n_locals as usize;

        // Arity check
        if has_rest {
            if argc < arity {
                return Err(SemaError::arity(
                    func.name
                        .map(resolve_spur)
                        .unwrap_or_else(|| "<lambda>".to_string()),
                    format!("{}+", arity),
                    argc,
                ));
            }
        } else if argc != arity {
            return Err(SemaError::arity(
                func.name
                    .map(resolve_spur)
                    .unwrap_or_else(|| "<lambda>".to_string()),
                arity.to_string(),
                argc,
            ));
        }

        // Copy args directly into current frame's base — no Vec allocation
        let func_idx = self.stack.len() - 1 - argc;
        let base = self.frames.last().unwrap().base;

        // Copy args into base slots (args are above base, no overlap issues)
        Self::copy_args_to_locals(&mut self.stack, base, func_idx + 1, arity, argc, has_rest);

        // Resize to exact local count (pads with nil or truncates excess)
        self.stack.resize(base + n_locals, Value::nil());

        // Replace current frame (reuse slot)
        let frame = self.frames.last_mut().unwrap();
        frame.closure = closure;
        frame.pc = 0;
        // base stays the same
        frame.open_upvalues = None;

        Ok(())
    }

    /// Copy args from the stack into local slots, handling rest params.
    /// `dst` is the base index for destination, `src` is the start of args.
    #[inline(always)]
    fn copy_args_to_locals(
        stack: &mut [Value],
        dst: usize,
        src: usize,
        arity: usize,
        argc: usize,
        has_rest: bool,
    ) {
        if has_rest {
            let rest: Vec<Value> = stack[src + arity..src + argc].to_vec();
            for i in 0..arity {
                stack[dst + i] = stack[src + i].clone();
            }
            stack[dst + arity] = Value::list(rest);
        } else {
            for i in 0..arity {
                stack[dst + i] = stack[src + i].clone();
            }
        }
    }

    // --- MakeClosure ---

    fn make_closure(&mut self) -> Result<(), SemaError> {
        // Read instruction operands from the current frame's bytecode.
        // We extract everything we need first, then release the borrow.
        let frame = self.frames.last().unwrap();
        let code = &frame.closure.func.chunk.code;
        let pc = frame.pc + 1;
        let func_id = u16::from_le_bytes([code[pc], code[pc + 1]]) as usize;
        let n_upvalues = u16::from_le_bytes([code[pc + 2], code[pc + 3]]) as usize;

        // Collect upvalue descriptors
        let mut uv_descs = Vec::with_capacity(n_upvalues);
        let mut uv_pc = pc + 4;
        for _ in 0..n_upvalues {
            let is_local = u16::from_le_bytes([code[uv_pc], code[uv_pc + 1]]);
            let idx = u16::from_le_bytes([code[uv_pc + 2], code[uv_pc + 3]]) as usize;
            uv_pc += 4;
            uv_descs.push((is_local != 0, idx));
        }

        let base = frame.base;
        let parent_upvalues = frame.closure.upvalues.clone();
        // Release the immutable borrow before mutating
        let _ = frame;

        let func = self.functions[func_id].clone();
        let mut upvalues = Vec::with_capacity(n_upvalues);

        for (is_local, idx) in &uv_descs {
            if *is_local {
                // Capture from current frame's local slot using a shared UpvalueCell.
                // Lazily allocate open_upvalues on first capture.
                let frame = self.frames.last_mut().unwrap();
                let n_locals = frame.closure.func.chunk.n_locals as usize;
                let open = frame
                    .open_upvalues
                    .get_or_insert_with(|| vec![None; n_locals]);
                let cell = if let Some(existing) = &open[*idx] {
                    existing.clone()
                } else {
                    let val = self.stack[base + *idx].clone();
                    let cell = Rc::new(UpvalueCell::new(val));
                    open[*idx] = Some(cell.clone());
                    cell
                };
                upvalues.push(cell);
            } else {
                // Capture from current frame's upvalue
                upvalues.push(parent_upvalues[*idx].clone());
            }
        }

        // Update pc past the entire instruction
        self.frames.last_mut().unwrap().pc = uv_pc;

        let closure = Rc::new(Closure { func, upvalues });
        let payload: Rc<dyn std::any::Any> = Rc::new(VmClosurePayload {
            closure: closure.clone(),
            functions: self.functions.clone(),
        });
        let closure_for_fallback = closure.clone();
        let functions = self.functions.clone();
        let globals = self.globals.clone();

        // The NativeFn wrapper is used as a fallback when called from outside the VM
        // (e.g., from stdlib HOFs like map/filter). Inside the VM, call_value detects
        // the payload and pushes a CallFrame instead — no Rust recursion.
        let native = Value::native_fn_from_rc(Rc::new(sema_core::NativeFn::with_payload(
            closure_for_fallback
                .func
                .name
                .map(resolve_spur)
                .unwrap_or_else(|| "<vm-closure>".to_string()),
            payload,
            move |ctx, args| {
                let mut vm = VM::new_with_rc_functions(globals.clone(), functions.clone());
                let func = &closure_for_fallback.func;
                let arity = func.arity as usize;
                let has_rest = func.has_rest;
                let n_locals = func.chunk.n_locals as usize;

                if has_rest {
                    if args.len() < arity {
                        return Err(SemaError::arity(
                            func.name
                                .map(resolve_spur)
                                .unwrap_or_else(|| "<lambda>".to_string()),
                            format!("{}+", arity),
                            args.len(),
                        ));
                    }
                } else if args.len() != arity {
                    return Err(SemaError::arity(
                        func.name
                            .map(resolve_spur)
                            .unwrap_or_else(|| "<lambda>".to_string()),
                        arity.to_string(),
                        args.len(),
                    ));
                }

                vm.stack.resize(n_locals, Value::nil());

                if has_rest {
                    for i in 0..arity {
                        vm.stack[i] = args.get(i).cloned().unwrap_or(Value::nil());
                    }
                    let rest: Vec<Value> = args[arity..].to_vec();
                    vm.stack[arity] = Value::list(rest);
                } else {
                    for i in 0..arity {
                        vm.stack[i] = args.get(i).cloned().unwrap_or(Value::nil());
                    }
                }

                vm.frames.push(CallFrame {
                    closure: closure_for_fallback.clone(),
                    pc: 0,
                    base: 0,
                    open_upvalues: None,
                });
                vm.run(ctx)
            },
        )));

        self.stack.push(native);
        Ok(())
    }

    // --- Exception handling ---

    #[cold]
    #[inline(never)]
    fn handle_exception(
        &mut self,
        err: SemaError,
        failing_pc: usize,
    ) -> Result<ExceptionAction, SemaError> {
        let mut pc_for_lookup = failing_pc as u32;
        // Walk frames from top looking for a handler
        while let Some(frame) = self.frames.last() {
            let chunk = &frame.closure.func.chunk;

            // Check exception table for this frame
            let mut found = None;
            for entry in &chunk.exception_table {
                if pc_for_lookup >= entry.try_start && pc_for_lookup < entry.try_end {
                    found = Some(entry.clone());
                    break;
                }
            }

            if let Some(entry) = found {
                // Restore stack to handler state
                let base = frame.base;
                self.stack.truncate(base + entry.stack_depth as usize);

                // Push error value as a map matching the tree-walker's error_to_value format
                let error_val = error_to_value(&err);
                self.stack.push(error_val);

                // Jump to handler
                let frame = self.frames.last_mut().unwrap();
                frame.pc = entry.handler_pc as usize;
                return Ok(ExceptionAction::Handled);
            }

            // No handler in this frame, pop it and try parent
            let frame = self.frames.pop().unwrap();
            self.stack.truncate(frame.base);
            // Parent frames use their own pc for lookup.
            // parent.pc is the *resume* PC (the byte after the CALL instruction).
            // Exception table intervals are half-open [try_start, try_end), so if the
            // CALL was the last instruction in the try body, parent.pc == try_end and
            // the lookup would miss. Subtract 1 to land inside the CALL instruction.
            if let Some(parent) = self.frames.last() {
                pc_for_lookup = parent.pc.saturating_sub(1) as u32;
            }
        }

        // No handler found anywhere
        Ok(ExceptionAction::Propagate(err))
    }

    // --- Debug inspection methods ---

    pub fn debug_frame_count(&self) -> usize {
        self.frames.len()
    }

    pub fn debug_stack_trace(&self) -> Vec<crate::debug::DapStackFrame> {
        self.frames
            .iter()
            .enumerate()
            .rev()
            .map(|(i, frame)| {
                let func = &frame.closure.func;
                let name = func
                    .name
                    .map(sema_core::resolve)
                    .unwrap_or_else(|| "<main>".to_string());
                let (line, col) = self.span_at_pc(frame);
                crate::debug::DapStackFrame {
                    id: i as u64,
                    name,
                    line,
                    column: col,
                    source_file: func.source_file.clone(),
                }
            })
            .collect()
    }

    pub fn debug_locals(&self, frame_idx: usize) -> Vec<crate::debug::DapVariable> {
        let Some(frame) = self.frames.get(frame_idx) else {
            return Vec::new();
        };
        let func = &frame.closure.func;
        let mut vars = Vec::new();
        for &(slot, spur) in &func.local_names {
            let idx = frame.base + slot as usize;
            let val = if let Some(ref open) = frame.open_upvalues {
                if let Some(Some(cell)) = open.get(slot as usize) {
                    cell.value.borrow().clone()
                } else {
                    self.stack.get(idx).cloned().unwrap_or(Value::nil())
                }
            } else {
                self.stack.get(idx).cloned().unwrap_or(Value::nil())
            };
            vars.push(crate::debug::DapVariable {
                name: sema_core::resolve(spur),
                value: sema_core::pretty_print(&val, 80),
                type_name: val.type_name().to_string(),
                variables_reference: 0,
            });
        }
        vars
    }

    pub fn debug_upvalues(&self, frame_idx: usize) -> Vec<crate::debug::DapVariable> {
        let Some(frame) = self.frames.get(frame_idx) else {
            return Vec::new();
        };
        frame
            .closure
            .upvalues
            .iter()
            .enumerate()
            .map(|(i, uv)| {
                let val = uv.value.borrow().clone();
                crate::debug::DapVariable {
                    name: format!("upvalue_{i}"),
                    value: sema_core::pretty_print(&val, 80),
                    type_name: val.type_name().to_string(),
                    variables_reference: 0,
                }
            })
            .collect()
    }

    pub fn debug_scopes(&self, frame_id: usize) -> Vec<crate::debug::DapScope> {
        let mut scopes = vec![crate::debug::DapScope {
            name: "Locals".to_string(),
            variables_reference: crate::debug::scope_locals_ref(frame_id),
            expensive: false,
        }];
        if !self.debug_upvalues(frame_id).is_empty() {
            scopes.push(crate::debug::DapScope {
                name: "Closure".to_string(),
                variables_reference: crate::debug::scope_upvalues_ref(frame_id),
                expensive: false,
            });
        }
        scopes
    }

    pub fn debug_variables(&self, reference: u64) -> Vec<crate::debug::DapVariable> {
        match crate::debug::decode_scope_ref(reference) {
            None => Vec::new(),
            Some(crate::debug::ScopeKind::Locals(frame_id)) => self.debug_locals(frame_id),
            Some(crate::debug::ScopeKind::Upvalues(frame_id)) => self.debug_upvalues(frame_id),
        }
    }

    fn span_at_pc(&self, frame: &CallFrame) -> (u64, u64) {
        let pc32 = frame.pc as u32;
        let spans = &frame.closure.func.chunk.spans;
        // Find the most recent span at or before the current PC
        match spans.binary_search_by_key(&pc32, |(p, _)| *p) {
            Ok(idx) => {
                let span = &spans[idx].1;
                (span.line as u64, span.col as u64 + 1)
            }
            Err(idx) if idx > 0 => {
                let span = &spans[idx - 1].1;
                (span.line as u64, span.col as u64 + 1)
            }
            _ => (0, 0),
        }
    }
}

enum ExceptionAction {
    Handled,
    Propagate(SemaError),
}

/// Convert a SemaError into a Sema map value, matching the tree-walker's format.
fn error_to_value(err: &SemaError) -> Value {
    let inner = err.inner();
    let mut map = BTreeMap::new();
    match inner {
        SemaError::Eval(msg) => {
            map.insert(Value::keyword("type"), Value::keyword("eval"));
            map.insert(Value::keyword("message"), Value::string(msg));
        }
        SemaError::Type { expected, got, .. } => {
            map.insert(Value::keyword("type"), Value::keyword("type-error"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("expected {expected}, got {got}")),
            );
            map.insert(Value::keyword("expected"), Value::string(expected));
            map.insert(Value::keyword("got"), Value::string(got));
        }
        SemaError::Arity {
            name,
            expected,
            got,
        } => {
            map.insert(Value::keyword("type"), Value::keyword("arity"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("{name} expects {expected} args, got {got}")),
            );
        }
        SemaError::Unbound(name) => {
            map.insert(Value::keyword("type"), Value::keyword("unbound"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("Unbound variable: {name}")),
            );
            map.insert(Value::keyword("name"), Value::string(name));
        }
        SemaError::UserException(val) => {
            map.insert(Value::keyword("type"), Value::keyword("user"));
            map.insert(Value::keyword("message"), Value::string(&val.to_string()));
            map.insert(Value::keyword("value"), val.clone());
        }
        SemaError::Io(msg) => {
            map.insert(Value::keyword("type"), Value::keyword("io"));
            map.insert(Value::keyword("message"), Value::string(msg));
        }
        SemaError::Llm(msg) => {
            map.insert(Value::keyword("type"), Value::keyword("llm"));
            map.insert(Value::keyword("message"), Value::string(msg));
        }
        SemaError::Reader { message, span } => {
            map.insert(Value::keyword("type"), Value::keyword("reader"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("{message} at {span}")),
            );
        }
        SemaError::PermissionDenied {
            function,
            capability,
        } => {
            map.insert(Value::keyword("type"), Value::keyword("permission-denied"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!(
                    "Permission denied: {function} requires '{capability}' capability"
                )),
            );
            map.insert(Value::keyword("function"), Value::string(function));
            map.insert(Value::keyword("capability"), Value::string(capability));
        }
        SemaError::PathDenied { function, path } => {
            map.insert(Value::keyword("type"), Value::keyword("permission-denied"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!(
                    "Permission denied: {function} — path '{path}' is outside allowed directories"
                )),
            );
            map.insert(Value::keyword("function"), Value::string(function));
            map.insert(Value::keyword("path"), Value::string(path));
        }
        SemaError::WithTrace { .. } | SemaError::WithContext { .. } => {
            unreachable!("inner() already unwraps these")
        }
    }
    Value::map(map)
}

// --- Arithmetic helpers ---

#[inline(always)]
fn vm_add(a: &Value, b: &Value) -> Result<Value, SemaError> {
    use sema_core::ValueView;
    match (a.view(), b.view()) {
        (ValueView::Int(x), ValueView::Int(y)) => Ok(Value::int(x.wrapping_add(y))),
        (ValueView::Float(x), ValueView::Float(y)) => Ok(Value::float(x + y)),
        (ValueView::Int(x), ValueView::Float(y)) => Ok(Value::float(x as f64 + y)),
        (ValueView::Float(x), ValueView::Int(y)) => Ok(Value::float(x + y as f64)),
        (ValueView::String(x), ValueView::String(y)) => {
            let mut s = (*x).clone();
            s.push_str(&y);
            Ok(Value::string(&s))
        }
        _ => Err(SemaError::type_error(
            "number or string",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

#[inline(always)]
fn vm_sub(a: &Value, b: &Value) -> Result<Value, SemaError> {
    use sema_core::ValueView;
    match (a.view(), b.view()) {
        (ValueView::Int(x), ValueView::Int(y)) => Ok(Value::int(x.wrapping_sub(y))),
        (ValueView::Float(x), ValueView::Float(y)) => Ok(Value::float(x - y)),
        (ValueView::Int(x), ValueView::Float(y)) => Ok(Value::float(x as f64 - y)),
        (ValueView::Float(x), ValueView::Int(y)) => Ok(Value::float(x - y as f64)),
        _ => Err(SemaError::type_error(
            "number",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

#[inline(always)]
fn vm_mul(a: &Value, b: &Value) -> Result<Value, SemaError> {
    use sema_core::ValueView;
    match (a.view(), b.view()) {
        (ValueView::Int(x), ValueView::Int(y)) => Ok(Value::int(x.wrapping_mul(y))),
        (ValueView::Float(x), ValueView::Float(y)) => Ok(Value::float(x * y)),
        (ValueView::Int(x), ValueView::Float(y)) => Ok(Value::float(x as f64 * y)),
        (ValueView::Float(x), ValueView::Int(y)) => Ok(Value::float(x * y as f64)),
        _ => Err(SemaError::type_error(
            "number",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

#[inline(always)]
fn vm_div(a: &Value, b: &Value) -> Result<Value, SemaError> {
    use sema_core::ValueView;
    match (a.view(), b.view()) {
        (ValueView::Int(_), ValueView::Int(0)) => Err(SemaError::eval("division by zero")),
        (ValueView::Int(x), ValueView::Int(y)) => {
            let result = x as f64 / y as f64;
            if result.fract() == 0.0 {
                Ok(Value::int(result as i64))
            } else {
                Ok(Value::float(result))
            }
        }
        (ValueView::Float(x), ValueView::Float(y)) => Ok(Value::float(x / y)),
        (ValueView::Int(x), ValueView::Float(y)) => Ok(Value::float(x as f64 / y)),
        (ValueView::Float(x), ValueView::Int(y)) => Ok(Value::float(x / y as f64)),
        _ => Err(SemaError::type_error(
            "number",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

/// Numeric-coercing equality: matches stdlib `=` semantics.
#[inline(always)]
fn vm_eq(a: &Value, b: &Value) -> bool {
    use sema_core::ValueView;
    match (a.view(), b.view()) {
        (ValueView::Int(x), ValueView::Int(y)) => x == y,
        (ValueView::Float(x), ValueView::Float(y)) => x == y,
        (ValueView::Int(x), ValueView::Float(y)) | (ValueView::Float(y), ValueView::Int(x)) => {
            (x as f64) == y
        }
        _ => a == b,
    }
}

fn vm_lt(a: &Value, b: &Value) -> Result<bool, SemaError> {
    use sema_core::ValueView;
    match (a.view(), b.view()) {
        (ValueView::Int(x), ValueView::Int(y)) => Ok(x < y),
        (ValueView::Float(x), ValueView::Float(y)) => Ok(x < y),
        (ValueView::Int(x), ValueView::Float(y)) => Ok((x as f64) < y),
        (ValueView::Float(x), ValueView::Int(y)) => Ok(x < (y as f64)),
        (ValueView::String(x), ValueView::String(y)) => Ok(x < y),
        _ => Err(SemaError::type_error(
            "comparable values",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

/// Compile a sequence of Value ASTs through the full pipeline and produce
/// the entry closure + function table ready for VM execution.
pub fn compile_program_with_spans(
    vals: &[Value],
    span_map: &sema_core::SpanMap,
) -> Result<(Rc<Closure>, Vec<Rc<Function>>), SemaError> {
    compile_program_with_spans_and_source(vals, span_map, None)
}

pub fn compile_program_with_spans_and_source(
    vals: &[Value],
    span_map: &sema_core::SpanMap,
    source_file: Option<std::path::PathBuf>,
) -> Result<(Rc<Closure>, Vec<Rc<Function>>), SemaError> {
    let mut resolved = Vec::new();
    let mut total_locals: u16 = 0;
    for val in vals {
        let core = crate::lower::lower_with_spans(val, span_map)?;
        let core = crate::optimize::optimize(core);
        let (res, n) = crate::resolve::resolve_with_locals(&core)?;
        total_locals = total_locals.max(n);
        resolved.push(res);
    }
    let result = crate::compiler::compile_many_with_locals(&resolved, total_locals)?;

    let functions: Vec<Rc<Function>> = result
        .functions
        .into_iter()
        .map(|mut f| {
            if f.source_file.is_none() {
                f.source_file = source_file.clone();
            }
            Rc::new(f)
        })
        .collect();
    let closure = Rc::new(Closure {
        func: Rc::new(Function {
            name: None,
            chunk: result.chunk,
            upvalue_descs: Vec::new(),
            arity: 0,
            has_rest: false,
            local_names: Vec::new(),
            source_file,
        }),
        upvalues: Vec::new(),
    });

    Ok((closure, functions))
}

/// Extract the set of source lines that have bytecode spans (valid breakpoint locations).
/// Includes spans from the main chunk and all sub-functions.
pub fn valid_breakpoint_lines(
    closure: &Closure,
    functions: &[Rc<Function>],
) -> Vec<u32> {
    let mut lines = std::collections::BTreeSet::new();
    for (_, s) in &closure.func.chunk.spans {
        lines.insert(s.line as u32);
    }
    for f in functions {
        for (_, s) in &f.chunk.spans {
            lines.insert(s.line as u32);
        }
    }
    lines.into_iter().collect()
}

/// Snap a requested breakpoint line to the nearest valid line with bytecode spans.
/// Prefers the same line, then searches forward, then backward.
/// Returns None if no valid lines exist.
pub fn snap_breakpoint_line(requested: u32, valid_lines: &[u32]) -> Option<u32> {
    if valid_lines.is_empty() {
        return None;
    }
    if valid_lines.contains(&requested) {
        return Some(requested);
    }
    // Binary search for insertion point
    let idx = valid_lines.partition_point(|&l| l < requested);
    let forward = valid_lines.get(idx).copied();
    let backward = if idx > 0 {
        valid_lines.get(idx - 1).copied()
    } else {
        None
    };
    match (forward, backward) {
        (Some(f), Some(b)) => {
            if (f - requested) <= (requested - b) {
                Some(f)
            } else {
                Some(b)
            }
        }
        (Some(f), None) => Some(f),
        (None, Some(b)) => Some(b),
        (None, None) => None,
    }
}

pub fn compile_program(vals: &[Value]) -> Result<(Rc<Closure>, Vec<Rc<Function>>), SemaError> {
    let mut resolved = Vec::new();
    let mut total_locals: u16 = 0;
    for val in vals {
        let core = crate::lower::lower(val)?;
        let core = crate::optimize::optimize(core);
        let (res, n) = crate::resolve::resolve_with_locals(&core)?;
        total_locals = total_locals.max(n);
        resolved.push(res);
    }
    let result = crate::compiler::compile_many_with_locals(&resolved, total_locals)?;

    let functions: Vec<Rc<Function>> = result.functions.into_iter().map(Rc::new).collect();
    let closure = Rc::new(Closure {
        func: Rc::new(Function {
            name: None,
            chunk: result.chunk,
            upvalue_descs: Vec::new(),
            arity: 0,
            has_rest: false,
            local_names: Vec::new(),
            source_file: None,
        }),
        upvalues: Vec::new(),
    });

    Ok((closure, functions))
}

/// Convenience: compile and run a string expression in the VM.
pub fn eval_str(input: &str, globals: &Rc<Env>, ctx: &EvalContext) -> Result<Value, SemaError> {
    let vals =
        sema_reader::read_many(input).map_err(|e| SemaError::eval(format!("parse error: {e}")))?;
    let (closure, functions) = compile_program(&vals)?;
    let mut vm = VM::new(globals.clone(), functions);
    vm.execute(closure, ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use sema_core::{intern, NativeFn};

    fn make_test_env() -> Rc<Env> {
        let env = Rc::new(Env::new());
        env.set(
            intern("+"),
            Value::native_fn(NativeFn::simple("+", |args| vm_add(&args[0], &args[1]))),
        );
        env.set(
            intern("-"),
            Value::native_fn(NativeFn::simple("-", |args| vm_sub(&args[0], &args[1]))),
        );
        env.set(
            intern("*"),
            Value::native_fn(NativeFn::simple("*", |args| vm_mul(&args[0], &args[1]))),
        );
        env.set(
            intern("/"),
            Value::native_fn(NativeFn::simple("/", |args| vm_div(&args[0], &args[1]))),
        );
        env.set(
            intern("="),
            Value::native_fn(NativeFn::simple("=", |args| {
                Ok(Value::bool(vm_eq(&args[0], &args[1])))
            })),
        );
        env.set(
            intern("<"),
            Value::native_fn(NativeFn::simple("<", |args| {
                Ok(Value::bool(vm_lt(&args[0], &args[1])?))
            })),
        );
        env.set(
            intern(">"),
            Value::native_fn(NativeFn::simple(">", |args| {
                Ok(Value::bool(vm_lt(&args[1], &args[0])?))
            })),
        );
        env.set(
            intern("not"),
            Value::native_fn(NativeFn::simple("not", |args| {
                Ok(Value::bool(!args[0].is_truthy()))
            })),
        );
        env.set(
            intern("list"),
            Value::native_fn(NativeFn::simple("list", |args| {
                Ok(Value::list(args.to_vec()))
            })),
        );
        env
    }

    fn eval(input: &str) -> Result<Value, SemaError> {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str(input, &globals, &ctx)
    }

    #[test]
    fn test_vm_int_literal() {
        assert_eq!(eval("42").unwrap(), Value::int(42));
    }

    #[test]
    fn test_vm_nil() {
        assert_eq!(eval("nil").unwrap(), Value::nil());
    }

    #[test]
    fn test_vm_bool() {
        assert_eq!(eval("#t").unwrap(), Value::bool(true));
        assert_eq!(eval("#f").unwrap(), Value::bool(false));
    }

    #[test]
    fn test_vm_string() {
        assert_eq!(eval("\"hello\"").unwrap(), Value::string("hello"));
    }

    #[test]
    fn test_vm_if_true() {
        assert_eq!(eval("(if #t 42 99)").unwrap(), Value::int(42));
    }

    #[test]
    fn test_vm_if_false() {
        assert_eq!(eval("(if #f 42 99)").unwrap(), Value::int(99));
    }

    #[test]
    fn test_vm_begin() {
        assert_eq!(eval("(begin 1 2 3)").unwrap(), Value::int(3));
    }

    #[test]
    fn test_vm_define_and_load() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define x 42)", &globals, &ctx).unwrap();
        let result = eval_str("x", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn test_vm_let() {
        assert_eq!(eval("(let ((x 10)) x)").unwrap(), Value::int(10));
    }

    #[test]
    fn test_vm_let_multiple() {
        assert_eq!(eval("(let ((x 10) (y 20)) y)").unwrap(), Value::int(20));
    }

    #[test]
    fn test_vm_nested_if() {
        assert_eq!(eval("(if (if #t #f #t) 1 2)").unwrap(), Value::int(2));
    }

    #[test]
    fn test_vm_lambda_call() {
        assert_eq!(eval("((lambda (x) x) 42)").unwrap(), Value::int(42));
    }

    #[test]
    fn test_vm_lambda_two_args() {
        assert_eq!(eval("((lambda (x y) y) 1 2)").unwrap(), Value::int(2));
    }

    #[test]
    fn test_vm_closure_capture() {
        assert_eq!(
            eval("(let ((x 10)) ((lambda () x)))").unwrap(),
            Value::int(10)
        );
    }

    #[test]
    fn test_vm_list_literal() {
        let result = eval("(list 1 2 3)").unwrap();
        let items = result.as_list().expect("Expected list");
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::int(1));
    }

    #[test]
    fn test_vm_make_vector() {
        let result = eval("[1 2 3]").unwrap();
        let items = result.as_vector().expect("Expected vector");
        assert_eq!(items.len(), 3);
    }

    #[test]
    fn test_vm_and_short_circuit() {
        assert_eq!(eval("(and #f 42)").unwrap(), Value::bool(false));
        assert_eq!(eval("(and #t 42)").unwrap(), Value::int(42));
    }

    #[test]
    fn test_vm_or_short_circuit() {
        assert_eq!(eval("(or 42 99)").unwrap(), Value::int(42));
        assert_eq!(eval("(or #f 99)").unwrap(), Value::int(99));
    }

    #[test]
    fn test_vm_throw_catch() {
        // Caught value is now a map with :type, :message, :value keys
        let result = eval("(try (throw \"boom\") (catch e (:value e)))").unwrap();
        assert_eq!(result, Value::string("boom"));
    }

    #[test]
    fn test_vm_throw_catch_type() {
        let result = eval("(try (throw \"boom\") (catch e (:type e)))").unwrap();
        assert_eq!(result, Value::keyword("user"));
    }

    #[test]
    fn test_vm_try_no_throw() {
        assert_eq!(eval("(try 42 (catch e 99))").unwrap(), Value::int(42));
    }

    #[test]
    fn test_vm_try_catch_native_error() {
        // Division by zero from NativeFn should be caught
        let result = eval("(try (/ 1 0) (catch e \"caught\"))").unwrap();
        assert_eq!(result, Value::string("caught"));
    }

    #[test]
    fn test_vm_try_catch_native_error_message() {
        let result = eval("(try (/ 1 0) (catch e (:message e)))").unwrap();
        let s = result.as_str().expect("Expected string");
        assert!(s.contains("division by zero"), "got: {s}");
    }

    #[test]
    fn test_vm_try_catch_type_error() {
        let result = eval("(try (+ 1 \"a\") (catch e (:type e)))").unwrap();
        assert_eq!(result, Value::keyword("type-error"));
    }

    #[test]
    fn test_vm_try_catch_from_closure_call() {
        // Regression: throw inside a called VM closure must be caught by try/catch
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define (thrower) (throw \"boom\"))", &globals, &ctx).unwrap();
        let result = eval_str("(try (thrower) (catch e \"caught\"))", &globals, &ctx).unwrap();
        assert_eq!(result, Value::string("caught"));
    }

    #[test]
    fn test_vm_try_catch_from_lambda_call() {
        // Throw from an immediately-called lambda
        let result = eval("(try ((fn () (throw 42))) (catch e (:value e)))").unwrap();
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn test_vm_try_catch_nested_call() {
        // Throw two calls deep
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define (inner) (throw \"deep\"))", &globals, &ctx).unwrap();
        eval_str("(define (outer) (inner))", &globals, &ctx).unwrap();
        let result = eval_str("(try (outer) (catch e \"caught\"))", &globals, &ctx).unwrap();
        assert_eq!(result, Value::string("caught"));
    }

    #[test]
    fn test_vm_try_catch_in_call_arg() {
        // Regression: try/catch as argument to another function must preserve stack
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define (thrower) (throw \"boom\"))", &globals, &ctx).unwrap();
        // try result used as arg to +
        let result = eval_str("(+ 1 (try (thrower) (catch e 2)))", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(3));
    }

    #[test]
    fn test_vm_try_catch_in_list_constructor() {
        // try/catch as one of several args — stack must be preserved for all
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define (thrower) (throw \"boom\"))", &globals, &ctx).unwrap();
        let result = eval_str("(list 1 2 (try (thrower) (catch e 3)) 4)", &globals, &ctx).unwrap();
        let items = result.as_list().expect("list");
        assert_eq!(items.len(), 4);
        assert_eq!(items[2], Value::int(3));
    }

    #[test]
    fn test_vm_try_catch_call_not_last() {
        // Call is not the last instruction in the try body
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define (thrower) (throw \"boom\"))", &globals, &ctx).unwrap();
        let result = eval_str(
            "(try (begin (thrower) 123) (catch e \"caught\"))",
            &globals,
            &ctx,
        )
        .unwrap();
        assert_eq!(result, Value::string("caught"));
    }

    #[test]
    fn test_vm_quote() {
        let result = eval("'(a b c)").unwrap();
        let items = result.as_list().expect("Expected list");
        assert_eq!(items.len(), 3);
    }

    #[test]
    fn test_vm_set() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define x 1)", &globals, &ctx).unwrap();
        eval_str("(set! x 42)", &globals, &ctx).unwrap();
        let result = eval_str("x", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn test_vm_recursive_define() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str(
            "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))",
            &globals,
            &ctx,
        )
        .unwrap();
        let result = eval_str("(fact 5)", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(120));
    }

    #[test]
    fn test_vm_do_loop() {
        let result = eval("(do ((i 0 (+ i 1))) ((= i 5) i))").unwrap();
        assert_eq!(result, Value::int(5));
    }

    #[test]
    fn test_vm_named_let() {
        let result =
            eval("(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))").unwrap();
        assert_eq!(result, Value::int(120));
    }

    #[test]
    fn test_vm_letrec() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        let result = eval_str(
            "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 4))",
            &globals,
            &ctx,
        ).unwrap();
        assert_eq!(result, Value::bool(true));
    }

    #[test]
    fn test_vm_rest_params() {
        let result = eval("((lambda (x . rest) rest) 1 2 3)").unwrap();
        let items = result.as_list().expect("Expected list");
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], Value::int(2));
        assert_eq!(items[1], Value::int(3));
    }

    // --- Task 8: Mutable upvalue tests ---

    #[test]
    fn test_vm_counter_closure() {
        // make-counter pattern: closure that mutates a captured variable
        let result =
            eval("(let ((n 0)) (let ((inc (lambda () (set! n (+ n 1)) n))) (inc) (inc) (inc)))")
                .unwrap();
        assert_eq!(result, Value::int(3));
    }

    #[test]
    fn test_vm_shared_mutable_upvalue() {
        // Two closures sharing the same mutable upvalue
        let result = eval(
            "(let ((n 0)) (let ((inc (lambda () (set! n (+ n 1)))) (get (lambda () n))) (inc) (inc) (get)))",
        )
        .unwrap();
        assert_eq!(result, Value::int(2));
    }

    #[test]
    fn test_vm_set_local_in_let() {
        // set! on a local variable (not captured)
        let result = eval("(let ((x 1)) (set! x 42) x)").unwrap();
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn test_vm_closure_captures_after_mutation() {
        // Closure captures value after mutation
        let result = eval("(let ((x 1)) (set! x 10) ((lambda () x)))").unwrap();
        assert_eq!(result, Value::int(10));
    }

    #[test]
    fn test_vm_closure_returns_closure() {
        // A closure that returns another closure
        let result = eval("(let ((f (lambda () (lambda (x) x)))) ((f) 42))").unwrap();
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn test_vm_make_adder() {
        // Classic make-adder pattern: closure captures upvalue
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str(
            "(define (make-adder n) (lambda (x) (+ n x)))",
            &globals,
            &ctx,
        )
        .unwrap();
        eval_str("(define add5 (make-adder 5))", &globals, &ctx).unwrap();
        let result = eval_str("(add5 3)", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(8));
    }

    #[test]
    fn test_vm_compose() {
        // compose: closure returns closure that captures two upvalues
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str(
            "(define (compose f g) (lambda (x) (f (g x))))",
            &globals,
            &ctx,
        )
        .unwrap();
        eval_str("(define inc (lambda (x) (+ x 1)))", &globals, &ctx).unwrap();
        eval_str("(define dbl (lambda (x) (* x 2)))", &globals, &ctx).unwrap();
        let result = eval_str("((compose dbl inc) 5)", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(12));
    }

    #[test]
    fn test_vm_nested_make_closure() {
        // Three levels deep
        let result = eval("((((lambda () (lambda () (lambda () 42))))))").unwrap();
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn test_vm_named_fn_rest_params() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define (f . args) args)", &globals, &ctx).unwrap();
        let result = eval_str("(f 1 2 3)", &globals, &ctx).unwrap();
        let items = result.as_list().expect("Expected list");
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::int(1));
    }

    #[test]
    fn test_vm_named_let_still_works_with_fix() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        let result = eval_str(
            "(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))",
            &globals,
            &ctx,
        )
        .unwrap();
        assert_eq!(result, Value::int(120));
    }

    #[test]
    fn test_vm_curry() {
        // Curry pattern
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str(
            "(define (curry f) (lambda (x) (lambda (y) (f x y))))",
            &globals,
            &ctx,
        )
        .unwrap();
        let result = eval_str("(((curry +) 3) 4)", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(7));
    }

    // --- Regression tests: division and equality semantics ---

    #[test]
    fn test_vm_div_int_returns_float_when_non_whole() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        let result = eval_str("(/ 3 2)", &globals, &ctx).unwrap();
        assert_eq!(result, Value::float(1.5));
    }

    #[test]
    fn test_vm_div_int_returns_int_when_whole() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        let result = eval_str("(/ 4 2)", &globals, &ctx).unwrap();
        assert_eq!(result, Value::int(2));
    }

    #[test]
    fn test_vm_div_int_negative_non_whole() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        let result = eval_str("(/ 7 3)", &globals, &ctx).unwrap();
        assert!(
            result.as_float().is_some(),
            "expected float, got {:?}",
            result
        );
    }

    #[test]
    fn test_vm_div_by_zero() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        assert!(eval_str("(/ 1 0)", &globals, &ctx).is_err());
    }

    #[test]
    fn test_vm_eq_int_float_coercion() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        assert_eq!(
            eval_str("(= 1 1.0)", &globals, &ctx).unwrap(),
            Value::bool(true)
        );
    }

    #[test]
    fn test_vm_eq_float_int_coercion() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        assert_eq!(
            eval_str("(= 1.0 1)", &globals, &ctx).unwrap(),
            Value::bool(true)
        );
    }

    #[test]
    fn test_vm_eq_int_float_not_equal() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        assert_eq!(
            eval_str("(= 1 2.0)", &globals, &ctx).unwrap(),
            Value::bool(false)
        );
    }

    #[test]
    fn test_vm_eq_same_type_int() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        assert_eq!(
            eval_str("(= 1 1)", &globals, &ctx).unwrap(),
            Value::bool(true)
        );
        assert_eq!(
            eval_str("(= 1 2)", &globals, &ctx).unwrap(),
            Value::bool(false)
        );
    }

    #[test]
    fn test_vm_eq_opcode_direct() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;
        let globals = Rc::new(Env::new());
        let ctx = EvalContext::new();
        let mut e = Emitter::new();
        e.emit_const(Value::int(1));
        e.emit_const(Value::float(1.0));
        e.emit_op(Op::Eq);
        e.emit_op(Op::Return);
        let func = Rc::new(crate::chunk::Function {
            name: None,
            chunk: e.into_chunk(),
            upvalue_descs: vec![],
            arity: 0,
            has_rest: false,
            local_names: vec![],
            source_file: None,
        });
        let closure = Rc::new(Closure {
            func,
            upvalues: vec![],
        });
        let mut vm = VM::new(globals, vec![]);
        let result = vm.execute(closure, &ctx).unwrap();
        assert_eq!(
            result,
            Value::bool(true),
            "Op::Eq should coerce int 1 == float 1.0"
        );
    }

    #[test]
    fn test_debug_hook_stops_at_span() {
        use crate::debug::{DebugCommand, DebugEvent, DebugState, StepMode};
        use std::sync::mpsc;

        let globals = make_test_env();
        let ctx = EvalContext::new();

        let input = "(+ 1 2)\n(+ 3 4)";
        let (vals, span_map) = sema_reader::read_many_with_spans(input).unwrap();
        let (closure, functions) = compile_program_with_spans(&vals, &span_map).unwrap();

        let (event_tx, event_rx) = mpsc::channel();
        let (cmd_tx, cmd_rx) = mpsc::channel();
        let mut debug_state = DebugState::new(event_tx, cmd_rx);
        debug_state.step_mode = StepMode::StepInto;

        // Send Continue commands so the VM doesn't block forever
        cmd_tx.send(DebugCommand::Continue).unwrap();
        cmd_tx.send(DebugCommand::Continue).unwrap();

        let mut vm = VM::new(globals, functions);
        let result = vm.execute_debug(closure, &ctx, &mut debug_state).unwrap();
        assert_eq!(result, Value::int(7));

        // Should have received at least one Stopped event
        let event = event_rx.try_recv();
        assert!(
            event.is_ok(),
            "should have received a Stopped event from debug hook"
        );
        match event.unwrap() {
            DebugEvent::Stopped { .. } => {} // expected
            other => panic!("expected Stopped event, got {other:?}"),
        }
    }

    #[test]
    fn test_vm_oob_jump_returns_error() {
        // Issue #1: A jump that goes past the end of bytecode should return
        // an error, not cause undefined behavior from unsafe pointer reads.
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let globals = Rc::new(Env::new());
        let ctx = EvalContext::new();
        let mut e = Emitter::new();
        // Emit a forward JUMP with an offset that goes way past the end
        e.emit_op(Op::Jump);
        e.emit_i32(1000); // jump to PC 1005, but code is only ~6 bytes
        e.emit_op(Op::Return);
        let func = Rc::new(crate::chunk::Function {
            name: None,
            chunk: e.into_chunk(),
            upvalue_descs: vec![],
            arity: 0,
            has_rest: false,
            local_names: vec![],
            source_file: None,
        });
        let closure = Rc::new(Closure {
            func,
            upvalues: vec![],
        });
        let mut vm = VM::new(globals, vec![]);
        let result = vm.execute(closure, &ctx);
        assert!(
            result.is_err(),
            "out-of-bounds jump should return an error, not UB"
        );
    }

    #[test]
    fn test_breakpoint_fires_on_each_loop_iteration() {
        // Issue #2: resume_skip prevents breakpoints from re-triggering in
        // single-line loops. After stopping at a breakpoint on a loop line,
        // "Continue" should stop again on the next iteration.
        use crate::debug::{DebugState, StepMode, VmExecResult};
        use std::path::PathBuf;

        let globals = make_test_env();
        let ctx = EvalContext::new();

        // A single-line loop: the loop body stays on line 1
        let input = "(let loop ((i 0)) (if (< i 5) (loop (+ i 1)) i))";
        let (vals, span_map) = sema_reader::read_many_with_spans(input).unwrap();
        let source_file = PathBuf::from("<test>");
        let (closure, functions) =
            compile_program_with_spans_and_source(&vals, &span_map, Some(source_file.clone()))
                .unwrap();

        let mut vm = VM::new(globals, functions);
        let mut debug = DebugState::new_headless();

        // Set breakpoint on line 1 (the only line)
        debug.set_breakpoints(&source_file, &[1]);
        debug.step_mode = StepMode::StepInto; // stop on entry first

        // Start: should stop on entry
        let result = vm
            .start_cooperative(closure, &ctx, &mut debug)
            .unwrap();
        assert!(
            matches!(result, VmExecResult::Stopped(_)),
            "should stop on entry"
        );

        // Continue: should hit the breakpoint on line 1
        debug.step_mode = StepMode::Continue;
        let result = vm.run_cooperative(&ctx, &mut debug).unwrap();
        assert!(
            matches!(result, VmExecResult::Stopped(_)),
            "should stop at breakpoint on first pass"
        );

        // Continue again: should hit breakpoint again on next iteration
        debug.step_mode = StepMode::Continue;
        let result = vm.run_cooperative(&ctx, &mut debug).unwrap();
        assert!(
            matches!(result, VmExecResult::Stopped(_)),
            "should stop at breakpoint on second iteration (resume_skip bug)"
        );
    }

    #[test]
    fn test_valid_breakpoint_lines_extraction() {
        // Verify that valid_breakpoint_lines correctly extracts lines with spans
        let code = "(+ 1 2)\n\n; comment\n(+ 3 4)";
        let (vals, span_map) = sema_reader::read_many_with_spans(code).unwrap();
        let (closure, functions) = compile_program_with_spans(&vals, &span_map).unwrap();
        let lines = valid_breakpoint_lines(&closure, &functions);

        assert!(lines.contains(&1), "line 1 (expr) should be valid");
        assert!(!lines.contains(&2), "line 2 (empty) should not be valid");
        assert!(!lines.contains(&3), "line 3 (comment) should not be valid");
        assert!(lines.contains(&4), "line 4 (expr) should be valid");
    }

    #[test]
    fn test_snap_breakpoint_line() {
        let valid = vec![1, 3, 5, 10];

        // Exact match
        assert_eq!(snap_breakpoint_line(3, &valid), Some(3));
        // Snap forward (closer)
        assert_eq!(snap_breakpoint_line(4, &valid), Some(5));
        // Equidistant between 1 and 3: prefers forward
        assert_eq!(snap_breakpoint_line(2, &valid), Some(3));
        // Equidistant: prefers forward
        assert_eq!(snap_breakpoint_line(4, &[3, 5]), Some(5));
        // Past the end: snaps to last
        assert_eq!(snap_breakpoint_line(20, &valid), Some(10));
        // Before the start: snaps to first
        assert_eq!(snap_breakpoint_line(0, &valid), Some(1));
        // Empty valid lines
        assert_eq!(snap_breakpoint_line(5, &[]), None);
    }

    #[test]
    fn test_bare_literal_breakpoint_snaps() {
        // Bare literals (like `42` or `"hello"`) don't get spans.
        // A breakpoint on a bare literal line should snap to the nearest line with spans.
        let code = "\"hello\"\n42\n(+ 1 2)";
        let (vals, span_map) = sema_reader::read_many_with_spans(code).unwrap();
        let (closure, functions) = compile_program_with_spans(&vals, &span_map).unwrap();
        let valid = valid_breakpoint_lines(&closure, &functions);

        // Lines 1 and 2 are bare literals — no spans
        assert!(!valid.contains(&1), "bare string should lack span");
        assert!(!valid.contains(&2), "bare int should lack span");
        assert!(valid.contains(&3), "function call should have span");

        // Snapping: line 1 and 2 should snap to line 3
        assert_eq!(snap_breakpoint_line(1, &valid), Some(3));
        assert_eq!(snap_breakpoint_line(2, &valid), Some(3));
    }

    #[test]
    fn test_global_redefinition_is_idempotent() {
        // Issue #3: The HTTP replay-restart strategy re-executes side effects.
        // Verify that re-defining globals doesn't error — (define x ...) twice
        // should just overwrite, not fail.
        let globals = make_test_env();
        let ctx = EvalContext::new();

        // First run: define x
        eval_str("(define x 42)", &globals, &ctx).unwrap();
        assert_eq!(
            eval_str("x", &globals, &ctx).unwrap(),
            Value::int(42)
        );

        // Second run (simulating replay): redefine x with same value
        eval_str("(define x 42)", &globals, &ctx).unwrap();
        assert_eq!(
            eval_str("x", &globals, &ctx).unwrap(),
            Value::int(42)
        );

        // Third run: redefine with different value (replay after mutation)
        eval_str("(define x 99)", &globals, &ctx).unwrap();
        assert_eq!(
            eval_str("x", &globals, &ctx).unwrap(),
            Value::int(99)
        );
    }

    #[test]
    fn test_spans_in_compiled_chunks() {
        let input = "(+ 1 2)\n(+ 3 4)";
        let (vals, span_map) = sema_reader::read_many_with_spans(input).unwrap();
        let (closure, _functions) = compile_program_with_spans(&vals, &span_map).unwrap();
        assert!(
            !closure.func.chunk.spans.is_empty(),
            "spans should be populated"
        );
        // Verify spans have correct line numbers
        let lines: Vec<u32> = closure
            .func
            .chunk
            .spans
            .iter()
            .map(|(_, s)| s.line as u32)
            .collect();
        assert!(lines.contains(&1), "should have span on line 1");
        assert!(lines.contains(&2), "should have span on line 2");
    }

    #[test]
    fn test_vm_div_opcode_direct() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;
        let globals = Rc::new(Env::new());
        let ctx = EvalContext::new();
        let mut e = Emitter::new();
        e.emit_const(Value::int(3));
        e.emit_const(Value::int(2));
        e.emit_op(Op::Div);
        e.emit_op(Op::Return);
        let func = Rc::new(crate::chunk::Function {
            name: None,
            chunk: e.into_chunk(),
            upvalue_descs: vec![],
            arity: 0,
            has_rest: false,
            local_names: vec![],
            source_file: None,
        });
        let closure = Rc::new(Closure {
            func,
            upvalues: vec![],
        });
        let mut vm = VM::new(globals, vec![]);
        let result = vm.execute(closure, &ctx).unwrap();
        assert_eq!(
            result,
            Value::float(1.5),
            "Op::Div 3/2 should return 1.5, not 1"
        );
    }
}
