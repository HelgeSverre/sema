use std::any::Any;
use std::cell::{Cell, RefCell};

use sema_core::{check_arity, SemaError, SemaStream, Value};

use crate::register_fn;

#[cfg(not(target_arch = "wasm32"))]
use sema_core::{Caps, Env, Sandbox};

// ── In-memory stream implementations ─────────────────────────────

/// Read/write byte buffer. Writes append; reads consume from position.
#[derive(Debug)]
struct ByteBufferStream {
    buf: RefCell<Vec<u8>>,
    pos: Cell<usize>,
}

impl ByteBufferStream {
    fn new(data: Vec<u8>) -> Self {
        ByteBufferStream {
            buf: RefCell::new(data),
            pos: Cell::new(0),
        }
    }

    fn empty() -> Self {
        Self::new(Vec::new())
    }
}

impl SemaStream for ByteBufferStream {
    fn read(&self, buf: &mut [u8]) -> Result<usize, SemaError> {
        let data = self.buf.borrow();
        let pos = self.pos.get();
        let available = data.len().saturating_sub(pos);
        let n = buf.len().min(available);
        buf[..n].copy_from_slice(&data[pos..pos + n]);
        self.pos.set(pos + n);
        Ok(n)
    }

    fn write(&self, data: &[u8]) -> Result<usize, SemaError> {
        self.buf.borrow_mut().extend_from_slice(data);
        Ok(data.len())
    }

    fn available(&self) -> Result<bool, SemaError> {
        Ok(self.pos.get() < self.buf.borrow().len())
    }

    fn stream_type(&self) -> &'static str {
        "byte-buffer"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Read-only stream from a string's UTF-8 bytes.
#[derive(Debug)]
struct StringStream {
    data: Vec<u8>,
    pos: Cell<usize>,
}

impl SemaStream for StringStream {
    fn read(&self, buf: &mut [u8]) -> Result<usize, SemaError> {
        let pos = self.pos.get();
        let available = self.data.len().saturating_sub(pos);
        let n = buf.len().min(available);
        buf[..n].copy_from_slice(&self.data[pos..pos + n]);
        self.pos.set(pos + n);
        Ok(n)
    }

    fn write(&self, _data: &[u8]) -> Result<usize, SemaError> {
        Err(SemaError::eval(
            "stream/write: stream is read-only (string stream)",
        ))
    }

    fn available(&self) -> Result<bool, SemaError> {
        Ok(self.pos.get() < self.data.len())
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn stream_type(&self) -> &'static str {
        "string"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── Helper to extract stream from args ───────────────────────────

fn expect_stream(
    args: &[Value],
    fname: &str,
    idx: usize,
) -> Result<std::rc::Rc<sema_core::StreamBox>, SemaError> {
    args[idx]
        .as_stream_rc()
        .ok_or_else(|| SemaError::type_error("stream", args[idx].type_name()))
        .map_err(|e| e.with_hint(format!("{fname} expects a stream as argument {}", idx + 1)))
}

// ── Registration ─────────────────────────────────────────────────

pub fn register(env: &sema_core::Env) {
    // --- predicate ---

    register_fn(env, "stream?", |args| {
        check_arity!(args, "stream?", 1);
        Ok(Value::bool(args[0].as_stream_rc().is_some()))
    });

    // --- core I/O ---

    register_fn(env, "stream/read", |args| {
        check_arity!(args, "stream/read", 2);
        let s = expect_stream(args, "stream/read", 0)?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        if n < 0 {
            return Err(SemaError::eval(format!(
                "stream/read: count must be non-negative, got {n}"
            )));
        }
        let mut buf = vec![0u8; n as usize];
        let bytes_read = s.read(&mut buf)?;
        buf.truncate(bytes_read);
        Ok(Value::bytevector(buf))
    });

    register_fn(env, "stream/write", |args| {
        check_arity!(args, "stream/write", 2);
        let s = expect_stream(args, "stream/write", 0)?;
        let data = args[1]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[1].type_name()))?;
        let n = s.write(data)?;
        Ok(Value::int(n as i64))
    });

    register_fn(env, "stream/read-byte", |args| {
        check_arity!(args, "stream/read-byte", 1);
        let s = expect_stream(args, "stream/read-byte", 0)?;
        let mut buf = [0u8; 1];
        let n = s.read(&mut buf)?;
        if n == 0 {
            Ok(Value::nil())
        } else {
            Ok(Value::int(buf[0] as i64))
        }
    });

    register_fn(env, "stream/write-byte", |args| {
        check_arity!(args, "stream/write-byte", 2);
        let s = expect_stream(args, "stream/write-byte", 0)?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        if !(0..=255).contains(&b) {
            return Err(SemaError::eval(format!(
                "stream/write-byte: value {b} out of range 0..255"
            )));
        }
        s.write(&[b as u8])?;
        Ok(Value::nil())
    });

    register_fn(env, "stream/available?", |args| {
        check_arity!(args, "stream/available?", 1);
        let s = expect_stream(args, "stream/available?", 0)?;
        Ok(Value::bool(s.available()?))
    });

    register_fn(env, "stream/close", |args| {
        check_arity!(args, "stream/close", 1);
        let s = expect_stream(args, "stream/close", 0)?;
        s.close()?;
        Ok(Value::nil())
    });

    register_fn(env, "stream/flush", |args| {
        check_arity!(args, "stream/flush", 1);
        let s = expect_stream(args, "stream/flush", 0)?;
        s.flush()?;
        Ok(Value::nil())
    });

    // --- introspection ---

    register_fn(env, "stream/readable?", |args| {
        check_arity!(args, "stream/readable?", 1);
        let s = expect_stream(args, "stream/readable?", 0)?;
        Ok(Value::bool(s.is_readable()))
    });

    register_fn(env, "stream/writable?", |args| {
        check_arity!(args, "stream/writable?", 1);
        let s = expect_stream(args, "stream/writable?", 0)?;
        Ok(Value::bool(s.is_writable()))
    });

    register_fn(env, "stream/type", |args| {
        check_arity!(args, "stream/type", 1);
        let s = expect_stream(args, "stream/type", 0)?;
        Ok(Value::string(s.stream_type()))
    });

    // --- constructors ---

    register_fn(env, "stream/byte-buffer", |args| {
        check_arity!(args, "stream/byte-buffer", 0);
        Ok(Value::stream(ByteBufferStream::empty()))
    });

    register_fn(env, "stream/from-string", |args| {
        check_arity!(args, "stream/from-string", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::stream(StringStream {
            data: s.as_bytes().to_vec(),
            pos: Cell::new(0),
        }))
    });

    register_fn(env, "stream/from-bytes", |args| {
        check_arity!(args, "stream/from-bytes", 1);
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        Ok(Value::stream(ByteBufferStream::new(bv.to_vec())))
    });

    // --- extraction ---

    register_fn(env, "stream/to-bytes", |args| {
        check_arity!(args, "stream/to-bytes", 1);
        let s = expect_stream(args, "stream/to-bytes", 0)?;
        let inner = s.borrow_inner();
        let any = inner.as_any();
        let buf = any.downcast_ref::<ByteBufferStream>().ok_or_else(|| {
            SemaError::eval(format!(
                "stream/to-bytes: expected byte-buffer stream, got {} stream",
                s.stream_type()
            ))
        })?;
        let bytes = buf.buf.borrow().clone();
        Ok(Value::bytevector(bytes))
    });

    register_fn(env, "stream/to-string", |args| {
        check_arity!(args, "stream/to-string", 1);
        let s = expect_stream(args, "stream/to-string", 0)?;
        let inner = s.borrow_inner();
        let any = inner.as_any();
        let buf = any.downcast_ref::<ByteBufferStream>().ok_or_else(|| {
            SemaError::eval(format!(
                "stream/to-string: expected byte-buffer stream, got {} stream",
                s.stream_type()
            ))
        })?;
        let bytes = buf.buf.borrow().clone();
        let text = std::str::from_utf8(&bytes)
            .map_err(|e| SemaError::eval(format!("stream/to-string: invalid UTF-8: {e}")))?;
        Ok(Value::string(text))
    });

    // --- convenience (no I/O, always available) ---

    register_fn(env, "stream/read-all", |args| {
        check_arity!(args, "stream/read-all", 1);
        let s = expect_stream(args, "stream/read-all", 0)?;
        let mut result = Vec::new();
        let mut buf = [0u8; 8192];
        loop {
            let n = s.read(&mut buf)?;
            if n == 0 {
                break;
            }
            result.extend_from_slice(&buf[..n]);
        }
        Ok(Value::bytevector(result))
    });

    register_fn(env, "stream/read-line", |args| {
        check_arity!(args, "stream/read-line", 1);
        let s = expect_stream(args, "stream/read-line", 0)?;
        let mut line = Vec::new();
        let mut buf = [0u8; 1];
        loop {
            let n = s.read(&mut buf)?;
            if n == 0 {
                // EOF
                if line.is_empty() {
                    return Ok(Value::nil());
                }
                break;
            }
            if buf[0] == b'\n' {
                break;
            }
            line.push(buf[0]);
        }
        // Strip trailing \r if present
        if line.last() == Some(&b'\r') {
            line.pop();
        }
        let text = String::from_utf8(line)
            .map_err(|e| SemaError::eval(format!("stream/read-line: invalid UTF-8: {e}")))?;
        Ok(Value::string(&text))
    });

    register_fn(env, "stream/write-string", |args| {
        check_arity!(args, "stream/write-string", 2);
        let s = expect_stream(args, "stream/write-string", 0)?;
        let text = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let n = s.write(text.as_bytes())?;
        Ok(Value::int(n as i64))
    });

    register_fn(env, "stream/copy", |args| {
        check_arity!(args, "stream/copy", 2);
        let src = expect_stream(args, "stream/copy", 0)?;
        let dst = expect_stream(args, "stream/copy", 1)?;
        let mut total: usize = 0;
        let mut buf = [0u8; 8192];
        loop {
            let n = src.read(&mut buf)?;
            if n == 0 {
                break;
            }
            dst.write(&buf[..n])?;
            total += n;
        }
        Ok(Value::int(total as i64))
    });
}

// ── File and stdio streams (not available on wasm) ───────────────

#[cfg(not(target_arch = "wasm32"))]
mod io_streams {
    use super::*;
    use std::io::{BufReader, BufWriter, Read, Write};

    /// Readable file stream with buffering.
    #[derive(Debug)]
    pub struct FileInputStream {
        reader: RefCell<BufReader<std::fs::File>>,
    }

    impl FileInputStream {
        pub fn open(path: &str) -> Result<Self, SemaError> {
            let file = std::fs::File::open(path)
                .map_err(|e| SemaError::eval(format!("stream/open-input: {path}: {e}")))?;
            Ok(FileInputStream {
                reader: RefCell::new(BufReader::new(file)),
            })
        }
    }

    impl SemaStream for FileInputStream {
        fn read(&self, buf: &mut [u8]) -> Result<usize, SemaError> {
            self.reader
                .borrow_mut()
                .read(buf)
                .map_err(|e| SemaError::eval(format!("stream/read: I/O error: {e}")))
        }

        fn write(&self, _data: &[u8]) -> Result<usize, SemaError> {
            Err(SemaError::eval(
                "stream/write: stream is read-only (file-input stream)",
            ))
        }

        fn available(&self) -> Result<bool, SemaError> {
            let reader = self.reader.borrow_mut();
            // Check if buffer has data; don't do a blocking read
            Ok(!reader.buffer().is_empty())
        }

        fn is_writable(&self) -> bool {
            false
        }

        fn stream_type(&self) -> &'static str {
            "file-input"
        }

        fn as_any(&self) -> &dyn Any {
            self
        }
    }

    /// Writable file stream with buffering.
    #[derive(Debug)]
    pub struct FileOutputStream {
        writer: RefCell<Option<BufWriter<std::fs::File>>>,
    }

    impl FileOutputStream {
        pub fn create(path: &str) -> Result<Self, SemaError> {
            let file = std::fs::File::create(path)
                .map_err(|e| SemaError::eval(format!("stream/open-output: {path}: {e}")))?;
            Ok(FileOutputStream {
                writer: RefCell::new(Some(BufWriter::new(file))),
            })
        }
    }

    impl SemaStream for FileOutputStream {
        fn read(&self, _buf: &mut [u8]) -> Result<usize, SemaError> {
            Err(SemaError::eval(
                "stream/read: stream is write-only (file-output stream)",
            ))
        }

        fn write(&self, data: &[u8]) -> Result<usize, SemaError> {
            let mut w = self.writer.borrow_mut();
            let writer = w
                .as_mut()
                .ok_or_else(|| SemaError::eval("stream/write: file stream is closed"))?;
            writer
                .write(data)
                .map_err(|e| SemaError::eval(format!("stream/write: I/O error: {e}")))
        }

        fn flush(&self) -> Result<(), SemaError> {
            let mut w = self.writer.borrow_mut();
            if let Some(writer) = w.as_mut() {
                writer
                    .flush()
                    .map_err(|e| SemaError::eval(format!("stream/flush: I/O error: {e}")))?;
            }
            Ok(())
        }

        fn close(&self) -> Result<(), SemaError> {
            self.flush()?;
            // Drop the writer to close the file
            *self.writer.borrow_mut() = None;
            Ok(())
        }

        fn is_readable(&self) -> bool {
            false
        }

        fn stream_type(&self) -> &'static str {
            "file-output"
        }

        fn as_any(&self) -> &dyn Any {
            self
        }
    }

    /// Stdin stream — readable, close is a no-op.
    #[derive(Debug)]
    pub struct StdinStream;

    impl SemaStream for StdinStream {
        fn read(&self, buf: &mut [u8]) -> Result<usize, SemaError> {
            std::io::stdin()
                .read(buf)
                .map_err(|e| SemaError::eval(format!("stream/read: stdin: {e}")))
        }

        fn write(&self, _data: &[u8]) -> Result<usize, SemaError> {
            Err(SemaError::eval("stream/write: *stdin* is read-only"))
        }

        fn is_writable(&self) -> bool {
            false
        }

        fn stream_type(&self) -> &'static str {
            "stdin"
        }

        fn as_any(&self) -> &dyn Any {
            self
        }
    }

    /// Stdout stream — writable, close is a no-op.
    #[derive(Debug)]
    pub struct StdoutStream;

    impl SemaStream for StdoutStream {
        fn read(&self, _buf: &mut [u8]) -> Result<usize, SemaError> {
            Err(SemaError::eval("stream/read: *stdout* is write-only"))
        }

        fn write(&self, data: &[u8]) -> Result<usize, SemaError> {
            std::io::stdout()
                .write(data)
                .map_err(|e| SemaError::eval(format!("stream/write: stdout: {e}")))
        }

        fn flush(&self) -> Result<(), SemaError> {
            std::io::stdout()
                .flush()
                .map_err(|e| SemaError::eval(format!("stream/flush: stdout: {e}")))
        }

        fn is_readable(&self) -> bool {
            false
        }

        fn stream_type(&self) -> &'static str {
            "stdout"
        }

        fn as_any(&self) -> &dyn Any {
            self
        }
    }

    /// Stderr stream — writable, close is a no-op.
    #[derive(Debug)]
    pub struct StderrStream;

    impl SemaStream for StderrStream {
        fn read(&self, _buf: &mut [u8]) -> Result<usize, SemaError> {
            Err(SemaError::eval("stream/read: *stderr* is write-only"))
        }

        fn write(&self, data: &[u8]) -> Result<usize, SemaError> {
            std::io::stderr()
                .write(data)
                .map_err(|e| SemaError::eval(format!("stream/write: stderr: {e}")))
        }

        fn flush(&self) -> Result<(), SemaError> {
            std::io::stderr()
                .flush()
                .map_err(|e| SemaError::eval(format!("stream/flush: stderr: {e}")))
        }

        fn is_readable(&self) -> bool {
            false
        }

        fn stream_type(&self) -> &'static str {
            "stderr"
        }

        fn as_any(&self) -> &dyn Any {
            self
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn register_io(env: &Env, sandbox: &Sandbox) {
    use io_streams::*;

    // --- file stream constructors (sandbox-gated) ---

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "stream/open-input",
        &[0],
        |args| {
            check_arity!(args, "stream/open-input", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::stream(FileInputStream::open(path)?))
        },
    );

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_WRITE,
        "stream/open-output",
        &[0],
        |args| {
            check_arity!(args, "stream/open-output", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::stream(FileOutputStream::create(path)?))
        },
    );

    // --- global stdio streams ---

    env.set(sema_core::intern("*stdin*"), Value::stream(StdinStream));
    env.set(sema_core::intern("*stdout*"), Value::stream(StdoutStream));
    env.set(sema_core::intern("*stderr*"), Value::stream(StderrStream));
}
