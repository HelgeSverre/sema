use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{resolve as resolve_spur, Env, EvalContext, SemaError, Spur, Value};

use crate::chunk::Function;
use crate::opcodes::Op;

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

/// A call frame in the VM's call stack.
struct CallFrame {
    closure: Rc<Closure>,
    pc: usize,
    base: usize,
    /// Open upvalue cells for locals in this frame.
    /// Maps local slot → shared UpvalueCell. Created lazily when a local is captured.
    open_upvalues: Vec<Option<Rc<UpvalueCell>>>,
}

/// The bytecode virtual machine.
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: Rc<Env>,
    functions: Vec<Rc<Function>>,
}

impl VM {
    pub fn new(globals: Rc<Env>, functions: Vec<Rc<Function>>) -> Self {
        VM {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            globals,
            functions,
        }
    }

    pub fn execute(&mut self, closure: Rc<Closure>, ctx: &EvalContext) -> Result<Value, SemaError> {
        let base = self.stack.len();
        // Reserve space for locals
        let n_locals = closure.func.chunk.n_locals as usize;
        for _ in 0..n_locals {
            self.stack.push(Value::Nil);
        }
        self.frames.push(CallFrame {
            closure,
            pc: 0,
            base,
            open_upvalues: vec![None; n_locals],
        });
        self.run(ctx)
    }

    fn run(&mut self, ctx: &EvalContext) -> Result<Value, SemaError> {
        loop {
            let frame = self.frames.last().unwrap();
            let code = &frame.closure.func.chunk.code;
            let pc = frame.pc;

            if pc >= code.len() {
                return Err(SemaError::eval("VM: pc out of bounds"));
            }

            let op = unsafe { std::mem::transmute::<u8, Op>(code[pc]) };

            match op {
                Op::Const => {
                    let idx = self.read_u16() as usize;
                    let frame = self.frames.last().unwrap();
                    let val = frame.closure.func.chunk.consts[idx].clone();
                    self.stack.push(val);
                }
                Op::Nil => {
                    self.advance_pc(1);
                    self.stack.push(Value::Nil);
                }
                Op::True => {
                    self.advance_pc(1);
                    self.stack.push(Value::Bool(true));
                }
                Op::False => {
                    self.advance_pc(1);
                    self.stack.push(Value::Bool(false));
                }
                Op::Pop => {
                    self.advance_pc(1);
                    self.stack.pop();
                }
                Op::Dup => {
                    self.advance_pc(1);
                    let val = self.stack.last().unwrap().clone();
                    self.stack.push(val);
                }

                // --- Locals ---
                Op::LoadLocal => {
                    let slot = self.read_u16() as usize;
                    let frame = self.frames.last().unwrap();
                    let val = if let Some(Some(cell)) = frame.open_upvalues.get(slot) {
                        cell.value.borrow().clone()
                    } else {
                        self.stack[frame.base + slot].clone()
                    };
                    self.stack.push(val);
                }
                Op::StoreLocal => {
                    let slot = self.read_u16() as usize;
                    let val = self.stack.pop().unwrap();
                    let frame = self.frames.last().unwrap();
                    let base = frame.base;
                    self.stack[base + slot] = val.clone();
                    // If there's an open upvalue for this slot, update it too
                    if let Some(Some(cell)) = frame.open_upvalues.get(slot) {
                        *cell.value.borrow_mut() = val;
                    }
                }

                // --- Upvalues ---
                Op::LoadUpvalue => {
                    let idx = self.read_u16() as usize;
                    let frame = self.frames.last().unwrap();
                    let val = frame.closure.upvalues[idx].value.borrow().clone();
                    self.stack.push(val);
                }
                Op::StoreUpvalue => {
                    let idx = self.read_u16() as usize;
                    let val = self.stack.pop().unwrap();
                    let frame = self.frames.last().unwrap();
                    *frame.closure.upvalues[idx].value.borrow_mut() = val;
                }

                // --- Globals ---
                Op::LoadGlobal => {
                    let spur = self.read_spur();
                    let val = self
                        .globals
                        .get(spur)
                        .ok_or_else(|| SemaError::Unbound(resolve_spur(spur)))?;
                    self.stack.push(val);
                }
                Op::StoreGlobal => {
                    let spur = self.read_spur();
                    let val = self.stack.pop().unwrap();
                    if !self.globals.set_existing(spur, val.clone()) {
                        self.globals.set(spur, val);
                    }
                }
                Op::DefineGlobal => {
                    let spur = self.read_spur();
                    let val = self.stack.pop().unwrap();
                    self.globals.set(spur, val);
                }

                // --- Control flow ---
                Op::Jump => {
                    let offset = self.read_i32();
                    let frame = self.frames.last_mut().unwrap();
                    frame.pc = (frame.pc as i64 + offset as i64) as usize;
                }
                Op::JumpIfFalse => {
                    let offset = self.read_i32();
                    let val = self.stack.pop().unwrap();
                    if !val.is_truthy() {
                        let frame = self.frames.last_mut().unwrap();
                        frame.pc = (frame.pc as i64 + offset as i64) as usize;
                    }
                }
                Op::JumpIfTrue => {
                    let offset = self.read_i32();
                    let val = self.stack.pop().unwrap();
                    if val.is_truthy() {
                        let frame = self.frames.last_mut().unwrap();
                        frame.pc = (frame.pc as i64 + offset as i64) as usize;
                    }
                }

                // --- Function calls ---
                Op::Call => {
                    let argc = self.read_u16() as usize;
                    self.call_value(argc, ctx)?;
                }
                Op::TailCall => {
                    let argc = self.read_u16() as usize;
                    self.tail_call_value(argc, ctx)?;
                }
                Op::Return => {
                    let result = self.stack.pop().unwrap_or(Value::Nil);
                    let frame = self.frames.pop().unwrap();
                    // Discard locals and function slot
                    self.stack.truncate(frame.base);
                    if self.frames.is_empty() {
                        return Ok(result);
                    }
                    self.stack.push(result);
                }

                // --- Closures ---
                Op::MakeClosure => {
                    self.make_closure()?;
                }

                Op::CallNative => {
                    let _native_id = self.read_u16();
                    let _argc = self.read_u16();
                    return Err(SemaError::eval("VM: CallNative not yet implemented"));
                }

                // --- Data constructors ---
                Op::MakeList => {
                    let n = self.read_u16() as usize;
                    let start = self.stack.len() - n;
                    let items: Vec<Value> = self.stack.drain(start..).collect();
                    self.stack.push(Value::List(Rc::new(items)));
                }
                Op::MakeVector => {
                    let n = self.read_u16() as usize;
                    let start = self.stack.len() - n;
                    let items: Vec<Value> = self.stack.drain(start..).collect();
                    self.stack.push(Value::Vector(Rc::new(items)));
                }
                Op::MakeMap => {
                    let n = self.read_u16() as usize;
                    let start = self.stack.len() - n * 2;
                    let items: Vec<Value> = self.stack.drain(start..).collect();
                    let mut map = BTreeMap::new();
                    for pair in items.chunks(2) {
                        map.insert(pair[0].clone(), pair[1].clone());
                    }
                    self.stack.push(Value::Map(Rc::new(map)));
                }
                Op::MakeHashMap => {
                    let n = self.read_u16() as usize;
                    let start = self.stack.len() - n * 2;
                    let items: Vec<Value> = self.stack.drain(start..).collect();
                    let mut map = hashbrown::HashMap::new();
                    for pair in items.chunks(2) {
                        map.insert(pair[0].clone(), pair[1].clone());
                    }
                    self.stack.push(Value::HashMap(Rc::new(map)));
                }

                // --- Exceptions ---
                Op::Throw => {
                    self.advance_pc(1);
                    let val = self.stack.pop().unwrap();
                    let err = SemaError::UserException(val);
                    // Try to find an exception handler in the current or enclosing frames
                    match self.handle_exception(err)? {
                        ExceptionAction::Handled => {}
                        ExceptionAction::Propagate(e) => return Err(e),
                    }
                }

                // --- Arithmetic ---
                Op::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(vm_add(&a, &b)?);
                }
                Op::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(vm_sub(&a, &b)?);
                }
                Op::Mul => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(vm_mul(&a, &b)?);
                }
                Op::Div => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(vm_div(&a, &b)?);
                }
                Op::Negate => {
                    let a = self.stack.pop().unwrap();
                    self.stack.push(match a {
                        Value::Int(n) => Value::Int(-n),
                        Value::Float(f) => Value::Float(-f),
                        _ => return Err(SemaError::type_error("number", a.type_name())),
                    });
                }
                Op::Not => {
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(!a.is_truthy()));
                }
                Op::Eq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a == b));
                }
                Op::Lt => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(vm_lt(&a, &b)?));
                }
                Op::Gt => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(vm_lt(&b, &a)?));
                }
                Op::Le => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(!vm_lt(&b, &a)?));
                }
                Op::Ge => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(!vm_lt(&a, &b)?));
                }

                // --- Specialized int fast paths ---
                Op::AddInt => {
                    self.advance_pc(1);
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => {
                            self.stack.push(Value::Int(x.wrapping_add(*y)));
                        }
                        _ => self.stack.push(vm_add(&a, &b)?),
                    }
                }
                Op::SubInt => {
                    self.advance_pc(1);
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => {
                            self.stack.push(Value::Int(x.wrapping_sub(*y)));
                        }
                        _ => self.stack.push(vm_sub(&a, &b)?),
                    }
                }
                Op::MulInt => {
                    self.advance_pc(1);
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => {
                            self.stack.push(Value::Int(x.wrapping_mul(*y)));
                        }
                        _ => self.stack.push(vm_mul(&a, &b)?),
                    }
                }
                Op::LtInt => {
                    self.advance_pc(1);
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => {
                            self.stack.push(Value::Bool(x < y));
                        }
                        _ => self.stack.push(Value::Bool(vm_lt(&a, &b)?)),
                    }
                }
                Op::EqInt => {
                    self.advance_pc(1);
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (&a, &b) {
                        (Value::Int(x), Value::Int(y)) => {
                            self.stack.push(Value::Bool(x == y));
                        }
                        _ => self.stack.push(Value::Bool(a == b)),
                    }
                }
            }
        }
    }

    // --- PC manipulation helpers ---

    fn advance_pc(&mut self, n: usize) {
        self.frames.last_mut().unwrap().pc += n;
    }

    /// Read a u16 operand and advance PC past the opcode + operand.
    fn read_u16(&mut self) -> u16 {
        let frame = self.frames.last_mut().unwrap();
        let code = &frame.closure.func.chunk.code;
        let pc = frame.pc + 1; // skip opcode
        let val = u16::from_le_bytes([code[pc], code[pc + 1]]);
        frame.pc = pc + 2;
        val
    }

    /// Read a u32 operand (for Spur globals) and advance PC.
    fn read_u32(&mut self) -> u32 {
        let frame = self.frames.last_mut().unwrap();
        let code = &frame.closure.func.chunk.code;
        let pc = frame.pc + 1;
        let val = u32::from_le_bytes([code[pc], code[pc + 1], code[pc + 2], code[pc + 3]]);
        frame.pc = pc + 4;
        val
    }

    /// Read a Spur from a u32 operand.
    fn read_spur(&mut self) -> Spur {
        let bits = self.read_u32();
        unsafe { std::mem::transmute::<u32, Spur>(bits) }
    }

    /// Read an i32 operand (for jump offsets) and advance PC.
    fn read_i32(&mut self) -> i32 {
        let frame = self.frames.last_mut().unwrap();
        let code = &frame.closure.func.chunk.code;
        let pc = frame.pc + 1;
        let val = i32::from_le_bytes([code[pc], code[pc + 1], code[pc + 2], code[pc + 3]]);
        frame.pc = pc + 4;
        val
    }

    // --- Function call implementation ---

    fn call_value(&mut self, argc: usize, ctx: &EvalContext) -> Result<(), SemaError> {
        let func_idx = self.stack.len() - 1 - argc;
        let func_val = self.stack[func_idx].clone();

        match func_val {
            Value::NativeFn(ref native) => {
                let args_start = func_idx + 1;
                let args: Vec<Value> = self.stack[args_start..].to_vec();
                self.stack.truncate(func_idx);
                let result = (native.func)(ctx, &args)?;
                self.stack.push(result);
                Ok(())
            }
            Value::Lambda(_) => {
                // Lambda from tree-walker — call via call_callback
                let args_start = func_idx + 1;
                let args: Vec<Value> = self.stack[args_start..].to_vec();
                self.stack.truncate(func_idx);
                let result = sema_core::call_callback(ctx, &func_val, &args)?;
                self.stack.push(result);
                Ok(())
            }
            Value::Keyword(kw) => {
                // Keyword as function: (kw map) -> map[kw]
                if argc != 1 {
                    return Err(SemaError::arity(resolve_spur(kw), "1", argc));
                }
                let arg = self.stack.pop().unwrap();
                self.stack.pop(); // pop keyword
                let result = match arg {
                    Value::Map(ref m) => m.get(&Value::Keyword(kw)).cloned().unwrap_or(Value::Nil),
                    Value::HashMap(ref m) => {
                        m.get(&Value::Keyword(kw)).cloned().unwrap_or(Value::Nil)
                    }
                    _ => return Err(SemaError::type_error("map or hashmap", arg.type_name())),
                };
                self.stack.push(result);
                Ok(())
            }
            _ => {
                // Check if it's a VM closure (stored as Value from MakeClosure)
                // For now, any other callable goes through call_callback
                let args_start = func_idx + 1;
                let args: Vec<Value> = self.stack[args_start..].to_vec();
                self.stack.truncate(func_idx);
                let result = sema_core::call_callback(ctx, &func_val, &args)?;
                self.stack.push(result);
                Ok(())
            }
        }
    }

    fn tail_call_value(&mut self, argc: usize, ctx: &EvalContext) -> Result<(), SemaError> {
        let func_idx = self.stack.len() - 1 - argc;
        let func_val = self.stack[func_idx].clone();

        match &func_val {
            Value::NativeFn(_) | Value::Lambda(_) | Value::Keyword(_) => {
                // Native/Lambda/Keyword: can't reuse frame, just do a regular call
                self.call_value(argc, ctx)
            }
            _ => {
                // For VM closures: we'd reuse the frame.
                // For now, fall back to regular call
                self.call_value(argc, ctx)
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
                // Capture from current frame's local slot using a shared UpvalueCell
                let frame = self.frames.last_mut().unwrap();
                let cell = if let Some(existing) = &frame.open_upvalues[*idx] {
                    existing.clone()
                } else {
                    let val = self.stack[base + *idx].clone();
                    let cell = Rc::new(UpvalueCell::new(val));
                    frame.open_upvalues[*idx] = Some(cell.clone());
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
        let closure_for_call = closure.clone();
        let functions = self.functions.clone();
        let globals = self.globals.clone();

        // We use Rc<RefCell> to allow the closure to store a reference to its own NativeFn value
        // for self-recursive calls (e.g., named-let loop functions).
        let self_ref: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
        let self_ref_inner = self_ref.clone();

        let native = Value::NativeFn(Rc::new(sema_core::NativeFn::with_ctx(
            closure_for_call
                .func
                .name
                .map(resolve_spur)
                .unwrap_or_else(|| "<vm-closure>".to_string()),
            move |ctx, args| {
                let mut vm = VM::new(globals.clone(), functions.clone());
                let func = &closure_for_call.func;
                let n_locals = func.chunk.n_locals as usize;
                let arity = func.arity as usize;
                let has_rest = func.has_rest;

                for _ in 0..n_locals {
                    vm.stack.push(Value::Nil);
                }

                if has_rest {
                    for i in 0..arity {
                        vm.stack[i] = args.get(i).cloned().unwrap_or(Value::Nil);
                    }
                    let rest: Vec<Value> = args[arity..].to_vec();
                    vm.stack[arity] = Value::List(Rc::new(rest));
                } else {
                    for i in 0..arity {
                        vm.stack[i] = args.get(i).cloned().unwrap_or(Value::Nil);
                    }
                }

                // If this is a named-let loop function, store ourselves in the loop name slot
                // (which is at slot `arity` since params are at 0..arity-1)
                if func.name.is_some() && arity < n_locals {
                    if let Some(self_val) = self_ref_inner.borrow().as_ref() {
                        vm.stack[arity] = self_val.clone();
                    }
                }

                vm.frames.push(CallFrame {
                    closure: Rc::new(Closure {
                        func: closure_for_call.func.clone(),
                        upvalues: closure_for_call.upvalues.clone(),
                    }),
                    pc: 0,
                    base: 0,
                    open_upvalues: vec![None; n_locals],
                });
                vm.run(ctx)
            },
        )));

        // Store self-reference for recursive calls
        *self_ref.borrow_mut() = Some(native.clone());

        self.stack.push(native);
        Ok(())
    }

    // --- Exception handling ---

    fn handle_exception(&mut self, err: SemaError) -> Result<ExceptionAction, SemaError> {
        // Walk frames from top looking for a handler
        while let Some(frame) = self.frames.last() {
            let pc = frame.pc as u32 - 1; // pc already advanced past the Throw
            let chunk = &frame.closure.func.chunk;

            // Check exception table for this frame
            let mut found = None;
            for entry in &chunk.exception_table {
                if pc >= entry.try_start && pc < entry.try_end {
                    found = Some(entry.clone());
                    break;
                }
            }

            if let Some(entry) = found {
                // Restore stack to handler state
                let base = frame.base;
                self.stack.truncate(base + entry.stack_depth as usize);

                // Push error value onto the stack for the handler's StoreLocal to consume
                let error_val = match &err {
                    SemaError::UserException(v) => v.clone(),
                    other => Value::String(Rc::new(other.to_string())),
                };
                self.stack.push(error_val);

                // Jump to handler
                let frame = self.frames.last_mut().unwrap();
                frame.pc = entry.handler_pc as usize;
                return Ok(ExceptionAction::Handled);
            }

            // No handler in this frame, pop it and try parent
            let frame = self.frames.pop().unwrap();
            self.stack.truncate(frame.base);
        }

        // No handler found anywhere
        Ok(ExceptionAction::Propagate(err))
    }
}

enum ExceptionAction {
    Handled,
    Propagate(SemaError),
}

// --- Arithmetic helpers ---

fn vm_add(a: &Value, b: &Value) -> Result<Value, SemaError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.wrapping_add(*y))),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
        (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 + y)),
        (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x + *y as f64)),
        (Value::String(x), Value::String(y)) => {
            let mut s = (**x).clone();
            s.push_str(y);
            Ok(Value::String(Rc::new(s)))
        }
        _ => Err(SemaError::type_error(
            "number or string",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

fn vm_sub(a: &Value, b: &Value) -> Result<Value, SemaError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.wrapping_sub(*y))),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
        (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 - y)),
        (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x - *y as f64)),
        _ => Err(SemaError::type_error(
            "number",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

fn vm_mul(a: &Value, b: &Value) -> Result<Value, SemaError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.wrapping_mul(*y))),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
        (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 * y)),
        (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x * *y as f64)),
        _ => Err(SemaError::type_error(
            "number",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

fn vm_div(a: &Value, b: &Value) -> Result<Value, SemaError> {
    match (a, b) {
        (Value::Int(_), Value::Int(0)) => Err(SemaError::eval("division by zero")),
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x / y)),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x / y)),
        (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 / y)),
        (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x / *y as f64)),
        _ => Err(SemaError::type_error(
            "number",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

fn vm_lt(a: &Value, b: &Value) -> Result<bool, SemaError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(x < y),
        (Value::Float(x), Value::Float(y)) => Ok(x < y),
        (Value::Int(x), Value::Float(y)) => Ok((*x as f64) < *y),
        (Value::Float(x), Value::Int(y)) => Ok(*x < (*y as f64)),
        (Value::String(x), Value::String(y)) => Ok(x < y),
        _ => Err(SemaError::type_error(
            "comparable values",
            format!("{} and {}", a.type_name(), b.type_name()),
        )),
    }
}

/// Convenience: compile and run a string expression in the VM.
pub fn eval_str(input: &str, globals: &Rc<Env>, ctx: &EvalContext) -> Result<Value, SemaError> {
    let vals =
        sema_reader::read_many(input).map_err(|e| SemaError::eval(format!("parse error: {e}")))?;
    let mut resolved = Vec::new();
    let mut total_locals: u16 = 0;
    for val in &vals {
        let core = crate::lower::lower(val)?;
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
        }),
        upvalues: Vec::new(),
    });

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
            Value::NativeFn(Rc::new(NativeFn::simple("+", |args| {
                vm_add(&args[0], &args[1])
            }))),
        );
        env.set(
            intern("-"),
            Value::NativeFn(Rc::new(NativeFn::simple("-", |args| {
                vm_sub(&args[0], &args[1])
            }))),
        );
        env.set(
            intern("*"),
            Value::NativeFn(Rc::new(NativeFn::simple("*", |args| {
                vm_mul(&args[0], &args[1])
            }))),
        );
        env.set(
            intern("/"),
            Value::NativeFn(Rc::new(NativeFn::simple("/", |args| {
                vm_div(&args[0], &args[1])
            }))),
        );
        env.set(
            intern("="),
            Value::NativeFn(Rc::new(NativeFn::simple("=", |args| {
                Ok(Value::Bool(args[0] == args[1]))
            }))),
        );
        env.set(
            intern("<"),
            Value::NativeFn(Rc::new(NativeFn::simple("<", |args| {
                Ok(Value::Bool(vm_lt(&args[0], &args[1])?))
            }))),
        );
        env.set(
            intern(">"),
            Value::NativeFn(Rc::new(NativeFn::simple(">", |args| {
                Ok(Value::Bool(vm_lt(&args[1], &args[0])?))
            }))),
        );
        env.set(
            intern("not"),
            Value::NativeFn(Rc::new(NativeFn::simple("not", |args| {
                Ok(Value::Bool(!args[0].is_truthy()))
            }))),
        );
        env.set(
            intern("list"),
            Value::NativeFn(Rc::new(NativeFn::simple("list", |args| {
                Ok(Value::List(Rc::new(args.to_vec())))
            }))),
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
        assert_eq!(eval("42").unwrap(), Value::Int(42));
    }

    #[test]
    fn test_vm_nil() {
        assert_eq!(eval("nil").unwrap(), Value::Nil);
    }

    #[test]
    fn test_vm_bool() {
        assert_eq!(eval("#t").unwrap(), Value::Bool(true));
        assert_eq!(eval("#f").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_vm_string() {
        assert_eq!(
            eval("\"hello\"").unwrap(),
            Value::String(Rc::new("hello".to_string()))
        );
    }

    #[test]
    fn test_vm_if_true() {
        assert_eq!(eval("(if #t 42 99)").unwrap(), Value::Int(42));
    }

    #[test]
    fn test_vm_if_false() {
        assert_eq!(eval("(if #f 42 99)").unwrap(), Value::Int(99));
    }

    #[test]
    fn test_vm_begin() {
        assert_eq!(eval("(begin 1 2 3)").unwrap(), Value::Int(3));
    }

    #[test]
    fn test_vm_define_and_load() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define x 42)", &globals, &ctx).unwrap();
        let result = eval_str("x", &globals, &ctx).unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_vm_let() {
        assert_eq!(eval("(let ((x 10)) x)").unwrap(), Value::Int(10));
    }

    #[test]
    fn test_vm_let_multiple() {
        assert_eq!(eval("(let ((x 10) (y 20)) y)").unwrap(), Value::Int(20));
    }

    #[test]
    fn test_vm_nested_if() {
        assert_eq!(eval("(if (if #t #f #t) 1 2)").unwrap(), Value::Int(2));
    }

    #[test]
    fn test_vm_lambda_call() {
        assert_eq!(eval("((lambda (x) x) 42)").unwrap(), Value::Int(42));
    }

    #[test]
    fn test_vm_lambda_two_args() {
        assert_eq!(eval("((lambda (x y) y) 1 2)").unwrap(), Value::Int(2));
    }

    #[test]
    fn test_vm_closure_capture() {
        assert_eq!(
            eval("(let ((x 10)) ((lambda () x)))").unwrap(),
            Value::Int(10)
        );
    }

    #[test]
    fn test_vm_list_literal() {
        let result = eval("(list 1 2 3)").unwrap();
        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::Int(1));
            }
            _ => panic!("Expected list, got {:?}", result),
        }
    }

    #[test]
    fn test_vm_make_vector() {
        let result = eval("[1 2 3]").unwrap();
        match result {
            Value::Vector(items) => assert_eq!(items.len(), 3),
            _ => panic!("Expected vector"),
        }
    }

    #[test]
    fn test_vm_and_short_circuit() {
        assert_eq!(eval("(and #f 42)").unwrap(), Value::Bool(false));
        assert_eq!(eval("(and #t 42)").unwrap(), Value::Int(42));
    }

    #[test]
    fn test_vm_or_short_circuit() {
        assert_eq!(eval("(or 42 99)").unwrap(), Value::Int(42));
        assert_eq!(eval("(or #f 99)").unwrap(), Value::Int(99));
    }

    #[test]
    fn test_vm_throw_catch() {
        let result = eval("(try (throw \"boom\") (catch e e))").unwrap();
        assert_eq!(result, Value::String(Rc::new("boom".to_string())));
    }

    #[test]
    fn test_vm_try_no_throw() {
        assert_eq!(eval("(try 42 (catch e 99))").unwrap(), Value::Int(42));
    }

    #[test]
    fn test_vm_quote() {
        let result = eval("'(a b c)").unwrap();
        match result {
            Value::List(items) => assert_eq!(items.len(), 3),
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_vm_set() {
        let globals = make_test_env();
        let ctx = EvalContext::new();
        eval_str("(define x 1)", &globals, &ctx).unwrap();
        eval_str("(set! x 42)", &globals, &ctx).unwrap();
        let result = eval_str("x", &globals, &ctx).unwrap();
        assert_eq!(result, Value::Int(42));
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
        assert_eq!(result, Value::Int(120));
    }

    #[test]
    fn test_vm_do_loop() {
        let result = eval("(do ((i 0 (+ i 1))) ((= i 5) i))").unwrap();
        assert_eq!(result, Value::Int(5));
    }

    #[test]
    fn test_vm_named_let() {
        let result =
            eval("(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))").unwrap();
        assert_eq!(result, Value::Int(120));
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
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_vm_rest_params() {
        let result = eval("((lambda (x . rest) rest) 1 2 3)").unwrap();
        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(items[0], Value::Int(2));
                assert_eq!(items[1], Value::Int(3));
            }
            _ => panic!("Expected list, got {:?}", result),
        }
    }
}
