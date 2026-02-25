use std::path::PathBuf;
use std::sync::mpsc as std_mpsc;

use tokio::io::BufReader;
use tokio::sync::mpsc as tokio_mpsc;

use sema_vm::debug::{DebugCommand, DebugEvent, DebugState};

use crate::protocol::{DapEvent, DapMessage, DapResponse};
use crate::transport;

/// Messages from the async frontend to the backend thread.
/// Only used for operations that require access to the backend thread's state.
enum BackendRequest {
    Launch {
        program: PathBuf,
        stop_on_entry: bool,
        cmd_rx: std_mpsc::Receiver<DebugCommand>,
    },
    SetBreakpoints {
        file: PathBuf,
        lines: Vec<u32>,
        reply: tokio_mpsc::Sender<Vec<u32>>,
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
    let mut dbg_cmd_tx: Option<std_mpsc::Sender<DebugCommand>> = None;

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
                            &mut dbg_cmd_tx,
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
    dbg_cmd_tx: &mut Option<std_mpsc::Sender<DebugCommand>>,
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
                let (cmd_tx, cmd_rx) = std_mpsc::channel::<DebugCommand>();
                let _ = backend_tx
                    .send(BackendRequest::Launch {
                        program,
                        stop_on_entry,
                        cmd_rx,
                    })
                    .await;
                *dbg_cmd_tx = Some(cmd_tx);
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

            let verified_ids = if let Some(ref tx) = dbg_cmd_tx {
                // VM is running or stopped — send via DebugCommand
                let (reply_tx, reply_rx) = std_mpsc::sync_channel(1);
                let _ = tx.send(DebugCommand::SetBreakpoints {
                    file,
                    lines: lines.clone(),
                    reply: reply_tx,
                });
                reply_rx.recv().unwrap_or_default()
            } else {
                // Pre-launch — send via backend
                let (reply_tx, mut reply_rx) = tokio_mpsc::channel(1);
                let _ = backend_tx
                    .send(BackendRequest::SetBreakpoints {
                        file,
                        lines: lines.clone(),
                        reply: reply_tx,
                    })
                    .await;
                reply_rx.recv().await.unwrap_or_default()
            };
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
            let frames = if let Some(ref tx) = dbg_cmd_tx {
                let (reply_tx, reply_rx) = std_mpsc::sync_channel(1);
                let _ = tx.send(DebugCommand::GetStackTrace { reply: reply_tx });
                reply_rx.recv().unwrap_or_default()
            } else {
                Vec::new()
            };
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
            let scopes = if let Some(ref tx) = dbg_cmd_tx {
                let (reply_tx, reply_rx) = std_mpsc::sync_channel(1);
                let _ = tx.send(DebugCommand::GetScopes {
                    frame_id,
                    reply: reply_tx,
                });
                reply_rx.recv().unwrap_or_default()
            } else {
                Vec::new()
            };
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
            let vars = if let Some(ref tx) = dbg_cmd_tx {
                let (reply_tx, reply_rx) = std_mpsc::sync_channel(1);
                let _ = tx.send(DebugCommand::GetVariables {
                    reference,
                    reply: reply_tx,
                });
                reply_rx.recv().unwrap_or_default()
            } else {
                Vec::new()
            };
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
            if let Some(ref tx) = dbg_cmd_tx {
                let _ = tx.send(DebugCommand::Continue);
            }
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
            if let Some(ref tx) = dbg_cmd_tx {
                let _ = tx.send(DebugCommand::StepOver);
            }
            send_response(stdout, seq, msg.seq, "next", None).await;
        }
        "stepIn" => {
            if let Some(ref tx) = dbg_cmd_tx {
                let _ = tx.send(DebugCommand::StepInto);
            }
            send_response(stdout, seq, msg.seq, "stepIn", None).await;
        }
        "stepOut" => {
            if let Some(ref tx) = dbg_cmd_tx {
                let _ = tx.send(DebugCommand::StepOut);
            }
            send_response(stdout, seq, msg.seq, "stepOut", None).await;
        }
        "pause" => {
            if let Some(ref tx) = dbg_cmd_tx {
                let _ = tx.send(DebugCommand::Pause);
            }
            send_response(stdout, seq, msg.seq, "pause", None).await;
        }
        "disconnect" => {
            if let Some(ref tx) = dbg_cmd_tx {
                let _ = tx.send(DebugCommand::Disconnect);
            }
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
    let mut vm: Option<sema_vm::VM> = None;
    let mut closure: Option<std::rc::Rc<sema_vm::Closure>> = None;
    let mut debug_state: Option<DebugState> = None;
    let mut interp: Option<sema_eval::Interpreter> = None;
    let mut pending_breakpoints: Vec<(PathBuf, Vec<u32>)> = Vec::new();

    loop {
        let req = rx.blocking_recv();
        let Some(req) = req else { break };

        match req {
            BackendRequest::Launch {
                program,
                stop_on_entry,
                cmd_rx,
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
                    match sema_vm::compile_program_with_spans_and_source(
                        &vals,
                        &span_map,
                        Some(program.clone()),
                    ) {
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

                // Create the event channel (VM → frontend)
                let (dbg_event_tx, dbg_event_rx) = std_mpsc::channel::<DebugEvent>();

                // Use the command receiver from the frontend
                let mut ds = DebugState::new(dbg_event_tx, cmd_rx);

                if stop_on_entry {
                    ds.step_mode = sema_vm::StepMode::StepInto;
                }

                // Apply pending breakpoints
                for (file, lines) in pending_breakpoints.drain(..) {
                    ds.set_breakpoints(&file, &lines);
                }

                closure = Some(compiled_closure.clone());
                let new_vm = sema_vm::VM::new(interpreter.global_env.clone(), functions);

                // Forward debug events from std_mpsc to tokio_mpsc in a separate thread
                let event_tx_fwd = event_tx.clone();
                std::thread::spawn(move || {
                    while let Ok(evt) = dbg_event_rx.recv() {
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

            BackendRequest::SetBreakpoints {
                file,
                lines,
                reply,
            } => {
                // Only used before launch (pending breakpoints)
                if let Some(ref mut ds) = debug_state {
                    let ids = ds.set_breakpoints(&file, &lines);
                    let _ = reply.blocking_send(ids);
                } else {
                    // Store for application at launch time, reply immediately
                    // with placeholder IDs so the frontend doesn't block
                    let count = lines.len();
                    pending_breakpoints.push((file, lines));
                    let ids: Vec<u32> = (1..=count as u32).collect();
                    let _ = reply.blocking_send(ids);
                }
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

            BackendRequest::Disconnect => {
                break;
            }
        }
    }
}
