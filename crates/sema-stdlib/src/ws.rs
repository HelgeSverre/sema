//! WebSocket client (`ws/*`). Native-only; gated on `Caps::NETWORK`.
//!
//! A connection is a `Value::Stream` wrapping [`WsConnection`] (a `SemaStream`),
//! so `with-open`/`stream/close` give RAII cleanup for free. The byte-stream
//! `read`/`write` methods are intentionally unsupported — the message-oriented
//! `ws/send`/`ws/recv` surface is the API.
//!
//! Mirrors the HTTP client's offload model (`http.rs`): a long-lived **pump**
//! task runs on the shared tokio runtime, bridging the socket to two channels —
//! an *unbounded* outgoing command channel and a *bounded* incoming event
//! channel. Top-level ops block the VM thread; ops inside an `async/spawn` task
//! yield `AwaitIo` so sibling tasks run while a recv/handshake is in flight.

#![cfg(not(target_arch = "wasm32"))]

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use futures::{SinkExt, StreamExt};
use tokio::sync::mpsc;
use tokio_tungstenite::tungstenite::Message;

use sema_core::{check_arity, Caps, IoHandle, IoPoll, SemaError, SemaStream, StreamBox, Value};

use crate::register_fn;

/// Capacity of the incoming-event channel. Bounded so a slow Sema consumer
/// applies back-pressure to the network read side instead of buffering without
/// limit. Outgoing commands use an *unbounded* channel so `ws/send` never blocks.
const EVENT_CAP: usize = 1024;

/// Outgoing command from the evaluator to the pump task.
enum WsFrame {
    Text(String),
    Binary(Vec<u8>),
    /// Graceful close: pump sends a Close frame and exits.
    Close,
}

/// Incoming event from the pump task to the evaluator.
enum WsEvent {
    Text(String),
    Binary(Vec<u8>),
    Close { code: u16, reason: String },
    Error(String),
}

/// A live client WebSocket. All I/O goes through the channels to the pump task;
/// the `SemaStream` byte methods are deliberately unsupported.
struct WsConnection {
    cmd_tx: mpsc::UnboundedSender<WsFrame>,
    evt_rx: Rc<RefCell<mpsc::Receiver<WsEvent>>>,
}

impl std::fmt::Debug for WsConnection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<websocket>")
    }
}

impl SemaStream for WsConnection {
    fn read(&self, _buf: &mut [u8]) -> Result<usize, SemaError> {
        Err(SemaError::eval("stream/read: not supported on a websocket")
            .with_hint("use (ws/recv conn) to receive messages"))
    }

    fn write(&self, _data: &[u8]) -> Result<usize, SemaError> {
        Err(
            SemaError::eval("stream/write: not supported on a websocket")
                .with_hint("use (ws/send conn msg) to send messages"),
        )
    }

    /// Best-effort graceful close: ask the pump to send a Close frame and exit.
    /// Idempotent — `StreamBox` guards against a double close. Dropping the
    /// connection value (last `cmd_tx`) also stops the pump, so cleanup happens
    /// even without an explicit close.
    fn close(&self) -> Result<(), SemaError> {
        let _ = self.cmd_tx.send(WsFrame::Close);
        Ok(())
    }

    /// The pump drops its `cmd_rx` when it exits (server close / error), which
    /// flips the sender to closed — so this tracks whether the socket is live.
    fn is_writable(&self) -> bool {
        !self.cmd_tx.is_closed()
    }

    fn is_readable(&self) -> bool {
        !self.cmd_tx.is_closed()
    }

    fn stream_type(&self) -> &'static str {
        "websocket"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

/// Resolve an argument to a websocket `StreamBox`, rejecting non-stream and
/// non-websocket streams with an actionable error.
fn ws_conn(args: &[Value], fname: &str, idx: usize) -> Result<Rc<StreamBox>, SemaError> {
    let arg = args
        .get(idx)
        .ok_or_else(|| SemaError::arity(fname, format!("{}", idx + 1), args.len()))?;
    let sb = arg.as_stream_rc().ok_or_else(|| {
        SemaError::type_error("websocket", arg.type_name()).with_hint(format!(
            "{fname} expects a websocket connection as argument {}",
            idx + 1
        ))
    })?;
    if sb
        .borrow_inner()
        .as_any()
        .downcast_ref::<WsConnection>()
        .is_none()
    {
        return Err(
            SemaError::type_error("websocket", sb.stream_type()).with_hint(format!(
                "{fname} expects a websocket, got a {} stream",
                sb.stream_type()
            )),
        );
    }
    Ok(sb)
}

/// Clone the channel handles out of a websocket `StreamBox`, releasing the inner
/// borrow before the caller does any (possibly blocking) channel I/O.
fn handles_of(
    sb: &StreamBox,
) -> (
    mpsc::UnboundedSender<WsFrame>,
    Rc<RefCell<mpsc::Receiver<WsEvent>>>,
) {
    let inner = sb.borrow_inner();
    let conn = inner.as_any().downcast_ref::<WsConnection>().unwrap();
    (conn.cmd_tx.clone(), conn.evt_rx.clone())
}

/// Translate a Sema value into an outgoing frame: string → text, bytevector →
/// binary, map → JSON-encoded text.
fn value_to_frame(v: &Value) -> Result<WsFrame, SemaError> {
    if let Some(s) = v.as_str() {
        return Ok(WsFrame::Text(s.to_string()));
    }
    if let Some(bv) = v.as_bytevector_rc() {
        return Ok(WsFrame::Binary(bv.to_vec()));
    }
    if v.as_map_rc().is_some() {
        let json = sema_core::value_to_json_lossy(v);
        let s = serde_json::to_string(&json)
            .map_err(|e| SemaError::eval(format!("ws/send: json encode: {e}")))?;
        return Ok(WsFrame::Text(s));
    }
    Err(SemaError::type_error("string, bytevector, or map", v.type_name()).with_hint(
        "ws/send accepts a string (text frame), a bytevector (binary frame), or a map (sent as JSON text)",
    ))
}

/// Decode an incoming event into the Sema value `ws/recv` returns. `None` (the
/// channel drained and disconnected) and a Close event both map to a value; an
/// Error event surfaces as a thrown `SemaError`.
fn event_to_value(ev: Option<WsEvent>) -> Result<Value, SemaError> {
    match ev {
        None => Ok(Value::nil()),
        Some(WsEvent::Text(s)) => Ok(tagged("text", Value::string(&s))),
        Some(WsEvent::Binary(b)) => Ok(tagged("binary", Value::bytevector(b))),
        Some(WsEvent::Close { code, reason }) => {
            let mut info = BTreeMap::new();
            info.insert(Value::keyword("code"), Value::int(code as i64));
            info.insert(Value::keyword("reason"), Value::string(&reason));
            Ok(tagged("close", Value::map(info)))
        }
        Some(WsEvent::Error(e)) => Err(SemaError::Io(e)),
    }
}

/// Build a single-key tagged map `{:<key> value}` (`{:text "hi"}`, `{:binary …}`).
fn tagged(key: &str, value: Value) -> Value {
    let mut m = BTreeMap::new();
    m.insert(Value::keyword(key), value);
    Value::map(m)
}

/// The pump task: connect, signal handshake result, then bridge the socket to
/// the command/event channels until either side closes.
async fn pump(
    url: String,
    mut cmd_rx: mpsc::UnboundedReceiver<WsFrame>,
    evt_tx: mpsc::Sender<WsEvent>,
    ready_tx: tokio::sync::oneshot::Sender<Result<(), String>>,
) {
    let ws = match tokio_tungstenite::connect_async(url.as_str()).await {
        Ok((ws, _resp)) => {
            let _ = ready_tx.send(Ok(()));
            ws
        }
        Err(e) => {
            let _ = ready_tx.send(Err(format!("ws/connect {url}: {e}")));
            return;
        }
    };

    let (mut sink, mut stream) = ws.split();
    loop {
        tokio::select! {
            cmd = cmd_rx.recv() => match cmd {
                Some(WsFrame::Text(s)) => {
                    if sink.send(Message::Text(s.into())).await.is_err() { break; }
                }
                Some(WsFrame::Binary(b)) => {
                    if sink.send(Message::Binary(b.into())).await.is_err() { break; }
                }
                // Explicit close, or the evaluator dropped the connection.
                Some(WsFrame::Close) | None => {
                    let _ = sink.send(Message::Close(None)).await;
                    break;
                }
            },
            msg = stream.next() => match msg {
                Some(Ok(Message::Text(t))) => {
                    if evt_tx.send(WsEvent::Text(t.to_string())).await.is_err() { break; }
                    sema_core::notify_io_complete();
                }
                Some(Ok(Message::Binary(b))) => {
                    if evt_tx.send(WsEvent::Binary(b.to_vec())).await.is_err() { break; }
                    sema_core::notify_io_complete();
                }
                Some(Ok(Message::Close(frame))) => {
                    let (code, reason) = match frame {
                        Some(f) => (u16::from(f.code), f.reason.to_string()),
                        None => (1005, String::new()), // 1005: no status present
                    };
                    let _ = evt_tx.send(WsEvent::Close { code, reason }).await;
                    sema_core::notify_io_complete();
                    break;
                }
                // Ping/Pong/raw frames: tungstenite auto-replies to pings.
                Some(Ok(_)) => {}
                Some(Err(e)) => {
                    let _ = evt_tx.send(WsEvent::Error(format!("websocket: {e}"))).await;
                    sema_core::notify_io_complete();
                    break;
                }
                // Stream ended with no close frame (abnormal).
                None => {
                    let _ = evt_tx
                        .send(WsEvent::Close { code: 1006, reason: "connection closed".to_string() })
                        .await;
                    sema_core::notify_io_complete();
                    break;
                }
            },
        }
    }
}

/// `ws/connect`: spawn the pump, then await the handshake (block at top level,
/// yield `AwaitIo` inside an async task). Returns the connection stream value.
fn ws_connect(url: &str) -> Result<Value, SemaError> {
    use tokio::sync::oneshot::error::TryRecvError;

    // Vestigial under CALL_NATIVE (the scheduler delivers the resume value), kept
    // for symmetry with the shipped async yield pattern.
    if let Some(v) = sema_core::take_resume_value() {
        return Ok(v);
    }

    let (cmd_tx, cmd_rx) = mpsc::unbounded_channel::<WsFrame>();
    let (evt_tx, evt_rx) = mpsc::channel::<WsEvent>(EVENT_CAP);
    let (ready_tx, mut ready_rx) = tokio::sync::oneshot::channel::<Result<(), String>>();

    let join =
        crate::async_rt::stdlib_shared_rt().spawn(pump(url.to_string(), cmd_rx, evt_tx, ready_tx));

    let conn_val = Value::stream(WsConnection {
        cmd_tx,
        evt_rx: Rc::new(RefCell::new(evt_rx)),
    });

    if sema_core::in_async_context() {
        // Park until the handshake completes; the pump's per-event notify wakes us.
        let abort_handle = join.abort_handle();
        let conn_for_poll = conn_val.clone();
        let handle = Rc::new(IoHandle::with_abort(
            move || match ready_rx.try_recv() {
                Err(TryRecvError::Empty) => IoPoll::Pending,
                Ok(Ok(())) => IoPoll::Ready(Ok(conn_for_poll.clone())),
                Ok(Err(msg)) => IoPoll::Ready(Err(msg)),
                Err(TryRecvError::Closed) => {
                    IoPoll::Ready(Err("ws/connect: connection worker dropped".to_string()))
                }
            },
            move || abort_handle.abort(),
        ));
        sema_core::set_yield_signal(sema_core::YieldReason::AwaitIo(handle));
        return Ok(Value::nil());
    }

    // Top level: block the VM thread on the handshake (it is not inside a runtime).
    match ready_rx.blocking_recv() {
        Ok(Ok(())) => Ok(conn_val),
        Ok(Err(msg)) => Err(SemaError::Io(msg)),
        Err(_) => Err(SemaError::eval(
            "ws/connect: connection worker dropped before handshake",
        )),
    }
}

/// The async-context recv path: yield an `AwaitIo` whose poller drains one event.
fn ws_recv_async(evt_rx: Rc<RefCell<mpsc::Receiver<WsEvent>>>) -> Result<Value, SemaError> {
    use tokio::sync::mpsc::error::TryRecvError;

    let handle = Rc::new(IoHandle::new(move || {
        match evt_rx.borrow_mut().try_recv() {
            Ok(ev) => match event_to_value(Some(ev)) {
                Ok(v) => IoPoll::Ready(Ok(v)),
                Err(e) => IoPoll::Ready(Err(e.to_string())),
            },
            Err(TryRecvError::Empty) => IoPoll::Pending,
            Err(TryRecvError::Disconnected) => IoPoll::Ready(Ok(Value::nil())),
        }
    }));
    sema_core::set_yield_signal(sema_core::YieldReason::AwaitIo(handle));
    Ok(Value::nil())
}

fn ws_send(args: &[Value]) -> Result<Value, SemaError> {
    check_arity!(args, "ws/send", 2);
    let sb = ws_conn(args, "ws/send", 0)?;
    if sb.is_closed() {
        return Err(SemaError::eval("ws/send: connection is closed"));
    }
    let frame = value_to_frame(&args[1])?;
    let (cmd_tx, _) = handles_of(&sb);
    cmd_tx.send(frame).map_err(|_| {
        SemaError::eval("ws/send: connection is closed")
            .with_hint("the websocket has stopped (server closed the connection or it errored)")
    })?;
    Ok(Value::nil())
}

fn ws_recv(args: &[Value]) -> Result<Value, SemaError> {
    check_arity!(args, "ws/recv", 1);
    let sb = ws_conn(args, "ws/recv", 0)?;
    let (_, evt_rx) = handles_of(&sb);

    if sema_core::in_async_context() {
        if let Some(v) = sema_core::take_resume_value() {
            return Ok(v);
        }
        return ws_recv_async(evt_rx);
    }

    // Top level: block until an event arrives or the channel disconnects.
    let ev = evt_rx.borrow_mut().blocking_recv();
    event_to_value(ev)
}

fn ws_close(args: &[Value]) -> Result<Value, SemaError> {
    check_arity!(args, "ws/close", 1);
    let sb = ws_conn(args, "ws/close", 0)?;
    sb.close()?;
    Ok(Value::nil())
}

fn ws_connected(args: &[Value]) -> Result<Value, SemaError> {
    check_arity!(args, "ws/connected?", 1);
    let sb = ws_conn(args, "ws/connected?", 0)?;
    let (cmd_tx, _) = handles_of(&sb);
    Ok(Value::bool(!sb.is_closed() && !cmd_tx.is_closed()))
}

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    // Establishing a connection touches the network → gate on NETWORK. The
    // per-message ops below operate on an already-open connection (which could
    // only be obtained through this gate), so they need no separate gate —
    // matching the server-side ws closures.
    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "ws/connect", |args| {
        check_arity!(args, "ws/connect", 1);
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        if !(url.starts_with("ws://") || url.starts_with("wss://")) {
            return Err(
                SemaError::eval(format!("ws/connect: not a websocket URL: {url}"))
                    .with_hint("the URL must start with ws:// or wss://"),
            );
        }
        ws_connect(url)
    });

    register_fn(env, "ws/send", ws_send);
    register_fn(env, "ws/recv", ws_recv);
    register_fn(env, "ws/close", ws_close);
    register_fn(env, "ws/connected?", ws_connected);
}
