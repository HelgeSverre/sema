use std::path::PathBuf;
use std::sync::mpsc as std_mpsc;

use tokio::io::BufReader;
use tokio::sync::mpsc as tokio_mpsc;

use sema_vm::debug::{DebugCommand, DebugEvent, DebugState};

use crate::protocol::{DapEvent, DapMessage, DapResponse};
use crate::transport;

/// Messages from the async frontend to the backend thread.
enum BackendRequest {
    Launch {
        program: PathBuf,
        stop_on_entry: bool,
    },
    SetBreakpoints {
        file: PathBuf,
        lines: Vec<u32>,
        reply: tokio_mpsc::Sender<Vec<u32>>,
    },
    Continue,
    StepIn,
    StepOver,
    StepOut,
    Pause,
    GetStackTrace {
        reply: tokio_mpsc::Sender<Vec<sema_vm::debug::DapStackFrame>>,
    },
    GetScopes {
        frame_id: usize,
        reply: tokio_mpsc::Sender<Vec<sema_vm::debug::DapScope>>,
    },
    GetVariables {
        reference: u64,
        reply: tokio_mpsc::Sender<Vec<sema_vm::debug::DapVariable>>,
    },
    ConfigurationDone,
    Disconnect,
}

pub async fn run() {
    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin);
    let mut stdout = tokio::io::stdout();

    let mut seq: u64 = 1;
    let (backend_tx, backend_rx) = tokio_mpsc::channel::<BackendRequest>(32);
    let (event_bridge_tx, mut event_bridge_rx) = tokio_mpsc::channel::<DebugEvent>(32);
    let mut launched = false;

    // Spawn the backend thread
    let event_bridge_tx_clone = event_bridge_tx.clone();
    std::thread::spawn(move || {
        backend_thread(backend_rx, event_bridge_tx_clone);
    });

    loop {
        tokio::select! {
            msg = transport::read_message(&mut reader) => {
                match msg {
                    Ok(Some(text)) => {
                        let parsed: Result<DapMessage, _> = serde_json::from_str(&text);
                        let Ok(msg) = parsed else {
                            eprintln!("DAP: failed to parse message: {text}");
                            continue;
                        };
                        let handled = handle_request(
                            &msg,
                            &mut stdout,
                            &mut seq,
                            &backend_tx,
                            &mut launched,
                        ).await;
                        if !handled {
                            break;
                        }
                    }
                    Ok(None) => break, // EOF
                    Err(e) => {
                        eprintln!("DAP: read error: {e}");
                        break;
                    }
                }
            }
            Some(event) = event_bridge_rx.recv() => {
                let dap_event = match event {
                    DebugEvent::Stopped { reason, description } => {
                        let reason_str = match reason {
                            sema_vm::debug::StopReason::Breakpoint => "breakpoint",
                            sema_vm::debug::StopReason::Step => "step",
                            sema_vm::debug::StopReason::Pause => "pause",
                            sema_vm::debug::StopReason::Entry => "entry",
                        };
                        DapEvent::new(seq, "stopped", Some(serde_json::json!({
                            "reason": reason_str,
                            "description": description,
                            "threadId": 1,
                            "allThreadsStopped": true,
                        })))
                    }
                    DebugEvent::Terminated => {
                        DapEvent::new(seq, "terminated", None)
                    }
                    DebugEvent::Output { category, output } => {
                        DapEvent::new(seq, "output", Some(serde_json::json!({
                            "category": category,
                            "output": output,
                        })))
                    }
                };
                seq += 1;
                let json = serde_json::to_string(&dap_event).unwrap();
                let _ = transport::write_message(&mut stdout, &json).await;
            }
        }
    }
}

async fn handle_request(
    msg: &DapMessage,
    stdout: &mut tokio::io::Stdout,
    seq: &mut u64,
    backend_tx: &tokio_mpsc::Sender<BackendRequest>,
    launched: &mut bool,
) -> bool {
    let Some(ref command) = msg.command else {
        return true;
    };

    match command.as_str() {
        "initialize" => {
            let body = serde_json::json!({
                "supportsConfigurationDoneRequest": true,
                "supportsFunctionBreakpoints": false,
                "supportsConditionalBreakpoints": false,
                "supportsStepBack": false,
                "supportsSetVariable": false,
                "supportsRestartFrame": false,
                "supportsModulesRequest": false,
                "supportsExceptionInfoRequest": false,
            });
            send_response(stdout, seq, msg.seq, "initialize", Some(body)).await;
            // Send initialized event
            let event = DapEvent::new(*seq, "initialized", None);
            *seq += 1;
            let json = serde_json::to_string(&event).unwrap();
            let _ = transport::write_message(stdout, &json).await;
        }
        "launch" => {
            let program = msg
                .arguments
                .as_ref()
                .and_then(|a| a.get("program"))
                .and_then(|p| p.as_str())
                .map(PathBuf::from);
            let stop_on_entry = msg
                .arguments
                .as_ref()
                .and_then(|a| a.get("stopOnEntry"))
                .and_then(|v| v.as_bool())
                .unwrap_or(false);

            if let Some(program) = program {
                let _ = backend_tx
                    .send(BackendRequest::Launch {
                        program,
                        stop_on_entry,
                    })
                    .await;
                *launched = true;
                send_response(stdout, seq, msg.seq, "launch", None).await;
            } else {
                send_error(stdout, seq, msg.seq, "launch", "missing 'program' argument").await;
            }
        }
        "setBreakpoints" => {
            let file = msg
                .arguments
                .as_ref()
                .and_then(|a| a.get("source"))
                .and_then(|s| s.get("path"))
                .and_then(|p| p.as_str())
                .map(PathBuf::from)
                .unwrap_or_default();
            let lines: Vec<u32> = msg
                .arguments
                .as_ref()
                .and_then(|a| a.get("breakpoints"))
                .and_then(|b| b.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|bp| bp.get("line").and_then(|l| l.as_u64()).map(|l| l as u32))
                        .collect()
                })
                .unwrap_or_default();

            let (reply_tx, mut reply_rx) = tokio_mpsc::channel(1);
            let _ = backend_tx
                .send(BackendRequest::SetBreakpoints {
                    file,
                    lines: lines.clone(),
                    reply: reply_tx,
                })
                .await;
            let verified_ids = reply_rx.recv().await.unwrap_or_default();
            let breakpoints: Vec<serde_json::Value> = lines
                .iter()
                .zip(verified_ids.iter())
                .map(|(line, id)| {
                    serde_json::json!({
                        "id": id,
                        "verified": true,
                        "line": line,
                    })
                })
                .collect();
            send_response(
                stdout,
                seq,
                msg.seq,
                "setBreakpoints",
                Some(serde_json::json!({ "breakpoints": breakpoints })),
            )
            .await;
        }
        "configurationDone" => {
            let _ = backend_tx.send(BackendRequest::ConfigurationDone).await;
            send_response(stdout, seq, msg.seq, "configurationDone", None).await;
        }
        "threads" => {
            send_response(
                stdout,
                seq,
                msg.seq,
                "threads",
                Some(serde_json::json!({
                    "threads": [{ "id": 1, "name": "main" }]
                })),
            )
            .await;
        }
        "stackTrace" => {
            let (reply_tx, mut reply_rx) = tokio_mpsc::channel(1);
            let _ = backend_tx
                .send(BackendRequest::GetStackTrace { reply: reply_tx })
                .await;
            let frames = reply_rx.recv().await.unwrap_or_default();
            let stack_frames: Vec<serde_json::Value> = frames
                .iter()
                .map(|f| {
                    let mut frame = serde_json::json!({
                        "id": f.id,
                        "name": f.name,
                        "line": f.line,
                        "column": f.column,
                    });
                    if let Some(ref path) = f.source_file {
                        frame.as_object_mut().unwrap().insert(
                            "source".to_string(),
                            serde_json::json!({
                                "name": path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default(),
                                "path": path.to_string_lossy(),
                            }),
                        );
                    }
                    frame
                })
                .collect();
            send_response(
                stdout,
                seq,
                msg.seq,
                "stackTrace",
                Some(serde_json::json!({
                    "stackFrames": stack_frames,
                    "totalFrames": stack_frames.len(),
                })),
            )
            .await;
        }
        "scopes" => {
            let frame_id = msg
                .arguments
                .as_ref()
                .and_then(|a| a.get("frameId"))
                .and_then(|f| f.as_u64())
                .unwrap_or(0) as usize;
            let (reply_tx, mut reply_rx) = tokio_mpsc::channel(1);
            let _ = backend_tx
                .send(BackendRequest::GetScopes {
                    frame_id,
                    reply: reply_tx,
                })
                .await;
            let scopes = reply_rx.recv().await.unwrap_or_default();
            let scope_json: Vec<serde_json::Value> = scopes
                .iter()
                .map(|s| {
                    serde_json::json!({
                        "name": s.name,
                        "variablesReference": s.variables_reference,
                        "expensive": s.expensive,
                    })
                })
                .collect();
            send_response(
                stdout,
                seq,
                msg.seq,
                "scopes",
                Some(serde_json::json!({ "scopes": scope_json })),
            )
            .await;
        }
        "variables" => {
            let reference = msg
                .arguments
                .as_ref()
                .and_then(|a| a.get("variablesReference"))
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            let (reply_tx, mut reply_rx) = tokio_mpsc::channel(1);
            let _ = backend_tx
                .send(BackendRequest::GetVariables {
                    reference,
                    reply: reply_tx,
                })
                .await;
            let vars = reply_rx.recv().await.unwrap_or_default();
            let var_json: Vec<serde_json::Value> = vars
                .iter()
                .map(|v| {
                    serde_json::json!({
                        "name": v.name,
                        "value": v.value,
                        "type": v.type_name,
                        "variablesReference": v.variables_reference,
                    })
                })
                .collect();
            send_response(
                stdout,
                seq,
                msg.seq,
                "variables",
                Some(serde_json::json!({ "variables": var_json })),
            )
            .await;
        }
        "continue" => {
            let _ = backend_tx.send(BackendRequest::Continue).await;
            send_response(
                stdout,
                seq,
                msg.seq,
                "continue",
                Some(serde_json::json!({ "allThreadsContinued": true })),
            )
            .await;
        }
        "next" => {
            let _ = backend_tx.send(BackendRequest::StepOver).await;
            send_response(stdout, seq, msg.seq, "next", None).await;
        }
        "stepIn" => {
            let _ = backend_tx.send(BackendRequest::StepIn).await;
            send_response(stdout, seq, msg.seq, "stepIn", None).await;
        }
        "stepOut" => {
            let _ = backend_tx.send(BackendRequest::StepOut).await;
            send_response(stdout, seq, msg.seq, "stepOut", None).await;
        }
        "pause" => {
            let _ = backend_tx.send(BackendRequest::Pause).await;
            send_response(stdout, seq, msg.seq, "pause", None).await;
        }
        "disconnect" => {
            let _ = backend_tx.send(BackendRequest::Disconnect).await;
            send_response(stdout, seq, msg.seq, "disconnect", None).await;
            return false;
        }
        other => {
            send_error(
                stdout,
                seq,
                msg.seq,
                other,
                &format!("unsupported command: {other}"),
            )
            .await;
        }
    }
    true
}

async fn send_response(
    stdout: &mut tokio::io::Stdout,
    seq: &mut u64,
    request_seq: u64,
    command: &str,
    body: Option<serde_json::Value>,
) {
    let resp = DapResponse::success(*seq, request_seq, command, body);
    *seq += 1;
    let json = serde_json::to_string(&resp).unwrap();
    let _ = transport::write_message(stdout, &json).await;
}

async fn send_error(
    stdout: &mut tokio::io::Stdout,
    seq: &mut u64,
    request_seq: u64,
    command: &str,
    message: &str,
) {
    let resp = DapResponse::error(*seq, request_seq, command, message);
    *seq += 1;
    let json = serde_json::to_string(&resp).unwrap();
    let _ = transport::write_message(stdout, &json).await;
}

// --- Backend thread ---

fn backend_thread(
    mut rx: tokio_mpsc::Receiver<BackendRequest>,
    event_tx: tokio_mpsc::Sender<DebugEvent>,
) {
    // Variables scope reference encoding:
    // frame_id * 2 + 1 = locals scope
    // frame_id * 2 + 2 = upvalues scope

    let mut vm: Option<sema_vm::VM> = None;
    let mut closure: Option<std::rc::Rc<sema_vm::Closure>> = None;
    let mut debug_state: Option<DebugState> = None;
    let mut interp: Option<sema_eval::Interpreter> = None;
    let mut dbg_cmd_tx: Option<std_mpsc::Sender<DebugCommand>> = None;
    let mut pending_breakpoints: Vec<(PathBuf, Vec<u32>, std_mpsc::SyncSender<Vec<u32>>)> =
        Vec::new();

    loop {
        let req = rx.blocking_recv();
        let Some(req) = req else { break };

        match req {
            BackendRequest::Launch {
                program,
                stop_on_entry,
            } => {
                let source = match std::fs::read_to_string(&program) {
                    Ok(s) => s,
                    Err(e) => {
                        let _ = event_tx.blocking_send(DebugEvent::Output {
                            category: "stderr".to_string(),
                            output: format!("Failed to read {}: {e}\n", program.display()),
                        });
                        let _ = event_tx.blocking_send(DebugEvent::Terminated);
                        continue;
                    }
                };

                let (vals, span_map) = match sema_reader::read_many_with_spans(&source) {
                    Ok(v) => v,
                    Err(e) => {
                        let _ = event_tx.blocking_send(DebugEvent::Output {
                            category: "stderr".to_string(),
                            output: format!("Parse error: {e}\n"),
                        });
                        let _ = event_tx.blocking_send(DebugEvent::Terminated);
                        continue;
                    }
                };

                let (compiled_closure, functions) =
                    match sema_vm::compile_program_with_spans(&vals, &span_map) {
                        Ok(v) => v,
                        Err(e) => {
                            let _ = event_tx.blocking_send(DebugEvent::Output {
                                category: "stderr".to_string(),
                                output: format!("Compile error: {e}\n"),
                            });
                            let _ = event_tx.blocking_send(DebugEvent::Terminated);
                            continue;
                        }
                    };

                // Set up the interpreter environment (provides stdlib, LLM, prelude)
                let interpreter = sema_eval::Interpreter::new();

                // Create channels for debugger communication
                let (dbg_event_tx, dbg_event_rx) = std_mpsc::channel::<DebugEvent>();
                let (cmd_tx, dbg_cmd_rx) = std_mpsc::channel::<DebugCommand>();
                dbg_cmd_tx = Some(cmd_tx);

                let mut ds = DebugState::new(dbg_event_tx, dbg_cmd_rx);

                if stop_on_entry {
                    ds.step_mode = sema_vm::StepMode::StepInto;
                }

                // Apply pending breakpoints
                for (file, lines, reply) in pending_breakpoints.drain(..) {
                    let ids = ds.set_breakpoints(&file, &lines);
                    let _ = reply.send(ids);
                }

                closure = Some(compiled_closure.clone());
                let new_vm = sema_vm::VM::new(interpreter.global_env.clone(), functions);

                // Forward debug events from std_mpsc to tokio_mpsc in a separate thread
                let event_tx_fwd = event_tx.clone();
                let dbg_event_rx_thread = dbg_event_rx;
                std::thread::spawn(move || {
                    while let Ok(evt) = dbg_event_rx_thread.recv() {
                        if event_tx_fwd.blocking_send(evt).is_err() {
                            break;
                        }
                    }
                });

                // Store state but don't run yet — wait for configurationDone
                debug_state = Some(ds);
                vm = Some(new_vm);
                interp = Some(interpreter);
            }

            BackendRequest::ConfigurationDone => {
                if let (Some(ref mut vm_inst), Some(ref cl), Some(ref mut ds), Some(ref interpreter)) =
                    (&mut vm, &closure, &mut debug_state, &interp)
                {
                    match vm_inst.execute_debug(cl.clone(), &interpreter.ctx, ds) {
                        Ok(val) => {
                            if !val.is_nil() {
                                let _ = event_tx.blocking_send(DebugEvent::Output {
                                    category: "stdout".to_string(),
                                    output: format!(
                                        "{}\n",
                                        sema_core::pretty_print(&val, 80)
                                    ),
                                });
                            }
                        }
                        Err(e) => {
                            let _ = event_tx.blocking_send(DebugEvent::Output {
                                category: "stderr".to_string(),
                                output: format!("Runtime error: {e}\n"),
                            });
                        }
                    }
                    let _ = event_tx.blocking_send(DebugEvent::Terminated);
                }
            }

            BackendRequest::SetBreakpoints {
                file,
                lines,
                reply,
            } => {
                if let Some(ref mut ds) = debug_state {
                    let ids = ds.set_breakpoints(&file, &lines);
                    let _ = reply.blocking_send(ids);
                } else {
                    // Store for later (before launch)
                    let (sync_tx, sync_rx) = std_mpsc::sync_channel(1);
                    pending_breakpoints.push((file, lines, sync_tx));
                    // Send back dummy IDs
                    std::thread::spawn(move || {
                        if let Ok(ids) = sync_rx.recv() {
                            let _ = reply.blocking_send(ids);
                        }
                    });
                }
            }

            BackendRequest::Continue => {
                if let Some(ref tx) = dbg_cmd_tx { let _ = tx.send(DebugCommand::Continue); }
            }
            BackendRequest::StepIn => {
                if let Some(ref tx) = dbg_cmd_tx { let _ = tx.send(DebugCommand::StepInto); }
            }
            BackendRequest::StepOver => {
                if let Some(ref tx) = dbg_cmd_tx { let _ = tx.send(DebugCommand::StepOver); }
            }
            BackendRequest::StepOut => {
                if let Some(ref tx) = dbg_cmd_tx { let _ = tx.send(DebugCommand::StepOut); }
            }
            BackendRequest::Pause => {
                if let Some(ref tx) = dbg_cmd_tx { let _ = tx.send(DebugCommand::Pause); }
            }

            BackendRequest::GetStackTrace { reply } => {
                let frames = vm
                    .as_ref()
                    .map(|v| v.debug_stack_trace())
                    .unwrap_or_default();
                let _ = reply.blocking_send(frames);
            }
            BackendRequest::GetScopes { frame_id, reply } => {
                let mut scopes = vec![sema_vm::debug::DapScope {
                    name: "Locals".to_string(),
                    variables_reference: (frame_id as u64) * 2 + 1,
                    expensive: false,
                }];
                if let Some(ref v) = vm {
                    if !v.debug_upvalues(frame_id).is_empty() {
                        scopes.push(sema_vm::debug::DapScope {
                            name: "Closure".to_string(),
                            variables_reference: (frame_id as u64) * 2 + 2,
                            expensive: false,
                        });
                    }
                }
                let _ = reply.blocking_send(scopes);
            }
            BackendRequest::GetVariables { reference, reply } => {
                let vars = if let Some(ref v) = vm {
                    if reference == 0 {
                        Vec::new()
                    } else if reference % 2 == 1 {
                        // Locals scope
                        let frame_id = ((reference - 1) / 2) as usize;
                        v.debug_locals(frame_id)
                    } else {
                        // Upvalues scope
                        let frame_id = ((reference - 2) / 2) as usize;
                        v.debug_upvalues(frame_id)
                    }
                } else {
                    Vec::new()
                };
                let _ = reply.blocking_send(vars);
            }

            BackendRequest::Disconnect => {
                if let Some(ref tx) = dbg_cmd_tx { let _ = tx.send(DebugCommand::Disconnect); }
                break;
            }
        }
    }
}
