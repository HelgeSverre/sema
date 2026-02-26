use std::path::PathBuf;
use std::sync::mpsc;

use sema_core::Value;

#[derive(Debug, Clone)]
pub struct DapStackFrame {
    pub id: u64,
    pub name: String,
    pub line: u64,
    pub column: u64,
    pub source_file: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct DapVariable {
    pub name: String,
    pub value: String,
    pub type_name: String,
    pub variables_reference: u64,
}

#[derive(Debug, Clone)]
pub struct DapScope {
    pub name: String,
    pub variables_reference: u64,
    pub expensive: bool,
}

/// Current stepping mode for the debugger.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepMode {
    /// Run until a breakpoint is hit.
    Continue,
    /// Stop at the next source line change (any frame depth).
    StepInto,
    /// Stop at the next source line change in the same or parent frame.
    StepOver,
    /// Stop when returning to the parent frame.
    StepOut,
}

/// Commands sent from the DAP frontend to the VM backend.
pub enum DebugCommand {
    Continue,
    StepInto,
    StepOver,
    StepOut,
    Pause,
    SetBreakpoints {
        file: PathBuf,
        lines: Vec<u32>,
        reply: mpsc::SyncSender<Vec<u32>>,
    },
    GetStackTrace {
        reply: mpsc::SyncSender<Vec<DapStackFrame>>,
    },
    GetScopes {
        frame_id: usize,
        reply: mpsc::SyncSender<Vec<DapScope>>,
    },
    GetVariables {
        reference: u64,
        reply: mpsc::SyncSender<Vec<DapVariable>>,
    },
    Disconnect,
}

/// Decoded scope variable reference.
pub enum ScopeKind {
    Locals(usize),
    Upvalues(usize),
}

/// Encode a locals scope reference for the given frame.
pub fn scope_locals_ref(frame_id: usize) -> u64 {
    (frame_id as u64) * 2 + 1
}

/// Encode an upvalues scope reference for the given frame.
pub fn scope_upvalues_ref(frame_id: usize) -> u64 {
    (frame_id as u64) * 2 + 2
}

/// Decode a scope variable reference into frame ID and kind.
pub fn decode_scope_ref(reference: u64) -> Option<ScopeKind> {
    if reference == 0 {
        return None;
    }
    if reference % 2 == 1 {
        Some(ScopeKind::Locals(((reference - 1) / 2) as usize))
    } else {
        Some(ScopeKind::Upvalues(((reference - 2) / 2) as usize))
    }
}

/// Events sent from the VM backend to the DAP frontend.
#[derive(Debug)]
pub enum DebugEvent {
    Stopped {
        reason: StopReason,
        description: Option<String>,
    },
    Terminated,
    Output {
        category: String,
        output: String,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum StopReason {
    Breakpoint,
    Step,
    Pause,
    Entry,
}

/// Mutable debugger state carried alongside the VM.
pub struct DebugState {
    /// Active breakpoints: (file_path, line) → breakpoint ID
    pub breakpoints: std::collections::HashMap<(PathBuf, u32), u32>,
    /// Current step mode
    pub step_mode: StepMode,
    /// Frame depth when stepping was initiated
    pub step_frame_depth: usize,
    /// Last source location we stopped at (file, line)
    pub last_stop_line: Option<(PathBuf, u32)>,
    /// External pause request (set by DAP frontend, checked by VM)
    pub pause_requested: bool,
    /// Skip debug stop checks while on the same line as last_stop_line.
    /// Set after returning Stopped; cleared when execution moves to a different line.
    pub resume_skip: bool,
    /// Instruction budget for cooperative yielding. When > 0, the VM will yield
    /// after executing approximately this many instructions. Set to 0 to disable.
    /// Decremented during execution; caller should reset before each resume.
    pub instructions_remaining: u32,
    /// Channel to send events to the DAP frontend
    pub event_tx: mpsc::Sender<DebugEvent>,
    /// Channel to receive commands from the DAP frontend
    pub command_rx: mpsc::Receiver<DebugCommand>,
    next_bp_id: u32,
}

impl DebugState {
    pub fn new(
        event_tx: mpsc::Sender<DebugEvent>,
        command_rx: mpsc::Receiver<DebugCommand>,
    ) -> Self {
        DebugState {
            breakpoints: std::collections::HashMap::new(),
            step_mode: StepMode::Continue,
            step_frame_depth: 0,
            last_stop_line: None,
            pause_requested: false,
            resume_skip: false,
            instructions_remaining: 0,
            event_tx,
            command_rx,
            next_bp_id: 1,
        }
    }

    /// Create a DebugState without functional channels.
    /// Used for cooperative (WASM) execution where commands are applied
    /// between `run_inner` calls, not via channels.
    pub fn new_headless() -> Self {
        let (event_tx, _) = mpsc::channel();
        let (_, command_rx) = mpsc::channel();
        DebugState {
            breakpoints: std::collections::HashMap::new(),
            step_mode: StepMode::Continue,
            step_frame_depth: 0,
            last_stop_line: None,
            pause_requested: false,
            resume_skip: false,
            instructions_remaining: 0,
            event_tx,
            command_rx,
            next_bp_id: 1,
        }
    }

    /// Check if we should stop at the given span and frame depth.
    pub fn should_stop(&self, file: Option<&PathBuf>, line: u32, frame_depth: usize) -> bool {
        if self.pause_requested {
            return true;
        }

        if let Some(f) = file {
            if self.breakpoints.contains_key(&(f.clone(), line)) {
                return true;
            }
        }

        match self.step_mode {
            StepMode::Continue => false,
            StepMode::StepInto => match &self.last_stop_line {
                Some((_, last_line)) => line != *last_line,
                None => true,
            },
            StepMode::StepOver => {
                frame_depth <= self.step_frame_depth
                    && match &self.last_stop_line {
                        Some((_, last_line)) => line != *last_line,
                        None => true,
                    }
            }
            StepMode::StepOut => frame_depth < self.step_frame_depth,
        }
    }

    /// Set breakpoints for a file, replacing any existing ones for that file.
    pub fn set_breakpoints(&mut self, file: &PathBuf, lines: &[u32]) -> Vec<u32> {
        self.breakpoints.retain(|(f, _), _| f != file);

        lines
            .iter()
            .map(|&line| {
                let id = self.next_bp_id;
                self.next_bp_id += 1;
                self.breakpoints.insert((file.clone(), line), id);
                id
            })
            .collect()
    }
}

/// Result of cooperative VM execution.
#[derive(Debug)]
pub enum VmExecResult {
    /// Execution completed normally with a return value.
    Finished(Value),
    /// Execution paused at a debug stop point.
    Stopped(StopInfo),
    /// Execution yielded after exhausting the instruction budget.
    /// Call `run_cooperative` again to continue.
    Yielded,
}

/// Information about why and where the VM stopped.
#[derive(Debug, Clone)]
pub struct StopInfo {
    pub reason: StopReason,
    pub file: Option<PathBuf>,
    pub line: u32,
}
