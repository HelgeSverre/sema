use std::path::PathBuf;
use std::sync::mpsc;

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
    Disconnect,
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

    /// Block the VM thread, send a Stopped event, and wait for a resume command.
    pub fn handle_stop(
        &mut self,
        file: Option<&PathBuf>,
        line: u32,
        frame_depth: usize,
        reason: StopReason,
    ) {
        self.last_stop_line = file.map(|f| (f.clone(), line));
        self.pause_requested = false;

        let _ = self.event_tx.send(DebugEvent::Stopped {
            reason,
            description: None,
        });

        loop {
            match self.command_rx.recv() {
                Ok(DebugCommand::Continue) => {
                    self.step_mode = StepMode::Continue;
                    break;
                }
                Ok(DebugCommand::StepInto) => {
                    self.step_mode = StepMode::StepInto;
                    self.step_frame_depth = frame_depth;
                    break;
                }
                Ok(DebugCommand::StepOver) => {
                    self.step_mode = StepMode::StepOver;
                    self.step_frame_depth = frame_depth;
                    break;
                }
                Ok(DebugCommand::StepOut) => {
                    self.step_mode = StepMode::StepOut;
                    self.step_frame_depth = frame_depth;
                    break;
                }
                Ok(DebugCommand::Pause) => {
                    // Already paused
                }
                Ok(DebugCommand::SetBreakpoints { file, lines, reply }) => {
                    let ids = self.set_breakpoints(&file, &lines);
                    let _ = reply.send(ids);
                }
                Ok(DebugCommand::Disconnect) => {
                    self.step_mode = StepMode::Continue;
                    break;
                }
                Err(_) => {
                    self.step_mode = StepMode::Continue;
                    break;
                }
            }
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
