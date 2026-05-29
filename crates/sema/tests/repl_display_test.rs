//! Regression test: REPL `display`/`print` output must survive the prompt
//! repaint on an interactive terminal.
//!
//! Background — `display`/`print` write to stdout with **no trailing newline**
//! (correct Scheme semantics), leaving the cursor mid-line. Under the old
//! rustyline-based REPL (<= v1.15.0) the next prompt was repainted with
//! `\r\x1b[K` (carriage-return + erase-to-end-of-line), which erased that
//! partial-line output before the user could see it: `(display "x")` appeared
//! to print nothing in the interactive REPL (it worked fine when piped). The
//! reedline migration (1.16.0) fixed it by doing a cursor-position-aware
//! repaint that pushes the prompt to a fresh line instead. This test guards
//! against a regression if the line editor or its repaint strategy changes.
//!
//! The bug only manifests on a real TTY — the piped/headless path is a
//! separate plain `BufRead` loop that emits no repaint escapes — so we drive
//! the binary through a pseudo-terminal and render the resulting escape stream
//! onto a virtual screen, then assert the output line survived.
//!
//! The terminal model below is a faithful port of a Python harness that was
//! verified to reproduce the erasure against a real v1.15.0 binary and to show
//! it fixed on a real 1.16.0 binary, so a future regression turns this red.

#![cfg(unix)]

use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use serial_test::serial;
use std::io::{Read, Write};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

/// A tiny terminal emulator: enough of the VT/CSI repertoire that `sema`'s REPL
/// actually emits (cursor moves, erase-line / erase-display, cursor-position
/// report) to render the final visible screen.
struct Screen {
    rows: usize,
    cols: usize,
    grid: Vec<Vec<char>>,
    cr: usize,
    cc: usize,
    pending: Vec<u8>,
}

impl Screen {
    fn new(rows: usize, cols: usize) -> Self {
        Screen {
            rows,
            cols,
            grid: vec![vec![' '; cols]; rows],
            cr: 0,
            cc: 0,
            pending: Vec::new(),
        }
    }

    fn clamp(&mut self) {
        if self.cr >= self.rows {
            self.cr = self.rows - 1;
        }
        if self.cc >= self.cols {
            self.cc = self.cols - 1;
        }
    }

    fn newline(&mut self) {
        self.cr += 1;
        if self.cr >= self.rows {
            self.grid.remove(0);
            self.grid.push(vec![' '; self.cols]);
            self.cr = self.rows - 1;
        }
    }

    fn put(&mut self, ch: char) {
        if self.cc >= self.cols {
            self.cc = 0;
            self.newline();
        }
        self.grid[self.cr][self.cc] = ch;
        self.cc += 1;
    }

    /// Feed bytes from the child. Returns any bytes that must be written back to
    /// the child (i.e. cursor-position reports answering a `\x1b[6n` query) —
    /// reedline blocks waiting for these, so a real terminal must reply.
    fn feed(&mut self, data: &[u8]) -> Vec<u8> {
        self.pending.extend_from_slice(data);
        let buf = std::mem::take(&mut self.pending);
        let n = buf.len();
        let mut dsr = Vec::new();
        let mut i = 0;
        while i < n {
            let b = buf[i];
            if b == 0x1b {
                if i + 1 >= n {
                    break; // incomplete escape; wait for more
                }
                match buf[i + 1] {
                    b'[' => {
                        // CSI: ESC [ params(0-9;?) final(letter)
                        let mut j = i + 2;
                        while j < n && (buf[j].is_ascii_digit() || buf[j] == b';' || buf[j] == b'?')
                        {
                            j += 1;
                        }
                        if j >= n {
                            break; // need the final byte
                        }
                        let params = std::str::from_utf8(&buf[i + 2..j]).unwrap_or("");
                        self.apply_csi(params, buf[j], &mut dsr);
                        i = j + 1;
                    }
                    // ESC 7 / ESC 8 (save/restore cursor) and any other 2-byte
                    // escape: skip — none affect the final visible text here.
                    _ => i += 2,
                }
            } else if b == b'\r' {
                self.cc = 0;
                i += 1;
            } else if b == b'\n' {
                self.newline();
                i += 1;
            } else if b == 0x08 {
                self.cc = self.cc.saturating_sub(1);
                i += 1;
            } else if b < 0x20 {
                i += 1; // other control byte
            } else {
                match decode_first_char(&buf[i..]) {
                    Some((ch, len)) => {
                        self.put(ch);
                        i += len;
                    }
                    None => break, // incomplete UTF-8 at boundary
                }
            }
        }
        self.pending = buf[i..].to_vec();
        dsr
    }

    fn apply_csi(&mut self, params: &str, final_byte: u8, dsr: &mut Vec<u8>) {
        let num = |idx: usize, default: usize| -> usize {
            params
                .split(';')
                .nth(idx)
                .filter(|s| !s.is_empty())
                .and_then(|s| s.parse().ok())
                .unwrap_or(default)
        };
        match final_byte {
            b'n' if params == "6" => {
                // Device Status Report — answer with the real cursor position.
                dsr.extend_from_slice(format!("\x1b[{};{}R", self.cr + 1, self.cc + 1).as_bytes());
            }
            b'H' | b'f' => {
                self.cr = num(0, 1).saturating_sub(1);
                self.cc = num(1, 1).saturating_sub(1);
                self.clamp();
            }
            b'J' => {
                // Erase in display (0 = cursor..end, 2 = all).
                match params {
                    "" | "0" => {
                        for x in self.cc..self.cols {
                            self.grid[self.cr][x] = ' ';
                        }
                        for y in (self.cr + 1)..self.rows {
                            for x in 0..self.cols {
                                self.grid[y][x] = ' ';
                            }
                        }
                    }
                    "2" => {
                        for y in 0..self.rows {
                            for x in 0..self.cols {
                                self.grid[y][x] = ' ';
                            }
                        }
                    }
                    _ => {}
                }
            }
            b'K' => {
                // Erase in line (0 = cursor..end, 1 = start..cursor, 2 = all).
                match params {
                    "" | "0" => {
                        for x in self.cc..self.cols {
                            self.grid[self.cr][x] = ' ';
                        }
                    }
                    "1" => {
                        for x in 0..=self.cc.min(self.cols - 1) {
                            self.grid[self.cr][x] = ' ';
                        }
                    }
                    "2" => {
                        for x in 0..self.cols {
                            self.grid[self.cr][x] = ' ';
                        }
                    }
                    _ => {}
                }
            }
            b'C' => {
                self.cc += num(0, 1);
                self.clamp();
            }
            b'D' => {
                self.cc = self.cc.saturating_sub(num(0, 1));
            }
            b'A' => {
                self.cr = self.cr.saturating_sub(num(0, 1));
            }
            b'B' => {
                self.cr += num(0, 1);
                self.clamp();
            }
            // SGR (m), mode set/reset (h/l), etc. — no effect on visible text.
            _ => {}
        }
    }

    fn render_lines(&self) -> Vec<String> {
        self.grid
            .iter()
            .map(|row| row.iter().collect::<String>().trim_end().to_string())
            .filter(|l| !l.is_empty())
            .collect()
    }
}

fn decode_first_char(bytes: &[u8]) -> Option<(char, usize)> {
    let b0 = *bytes.first()?;
    let len = if b0 < 0x80 {
        1
    } else if b0 >> 5 == 0b110 {
        2
    } else if b0 >> 4 == 0b1110 {
        3
    } else if b0 >> 3 == 0b11110 {
        4
    } else {
        1 // stray continuation byte
    };
    if bytes.len() < len {
        return None; // incomplete multi-byte char at chunk boundary
    }
    match std::str::from_utf8(&bytes[..len]) {
        Ok(s) => s.chars().next().map(|c| (c, len)),
        Err(_) => Some(('\u{FFFD}', 1)),
    }
}

/// Run the interactive REPL inside a pty, type `cmds` (each followed by Enter),
/// then `,quit`, and return the final rendered screen as visible lines.
fn run_repl(cmds: &[&str]) -> Vec<String> {
    let pty = native_pty_system();
    let pair = pty
        .openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .expect("openpty");

    let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_sema"));
    cmd.arg("--no-llm");
    cmd.env("TERM", "xterm-256color");
    let mut child = pair.slave.spawn_command(cmd).expect("spawn sema");
    drop(pair.slave);

    let mut reader = pair.master.try_clone_reader().expect("clone reader");
    let writer = Arc::new(Mutex::new(pair.master.take_writer().expect("take writer")));

    let screen = Arc::new(Mutex::new(Screen::new(24, 80)));

    let screen_r = screen.clone();
    let writer_r = writer.clone();
    let (done_tx, done_rx) = mpsc::channel::<()>();
    let reader_thread = thread::spawn(move || {
        let mut buf = [0u8; 4096];
        loop {
            match reader.read(&mut buf) {
                Ok(0) | Err(_) => break,
                Ok(n) => {
                    let dsr = screen_r.lock().unwrap().feed(&buf[..n]);
                    if !dsr.is_empty() {
                        let mut w = writer_r.lock().unwrap();
                        let _ = w.write_all(&dsr);
                        let _ = w.flush();
                    }
                }
            }
        }
        let _ = done_tx.send(());
    });

    // Let the banner + first prompt settle.
    thread::sleep(Duration::from_millis(800));
    for c in cmds {
        {
            let mut w = writer.lock().unwrap();
            w.write_all(c.as_bytes()).unwrap();
            w.write_all(b"\n").unwrap();
            w.flush().unwrap();
        }
        thread::sleep(Duration::from_millis(500));
    }
    {
        let mut w = writer.lock().unwrap();
        let _ = w.write_all(b",quit\n");
        let _ = w.flush();
    }

    let _ = done_rx.recv_timeout(Duration::from_secs(5));
    let _ = child.kill();
    let _ = reader_thread.join();

    let lines = screen.lock().unwrap().render_lines();
    lines
}

#[test]
#[serial]
fn display_output_survives_prompt_repaint() {
    let sentinel = "SEMA_DISPLAY_SENTINEL";
    let lines = run_repl(&[&format!("(display \"{sentinel}\")"), "(+ 1 2)"]);
    let screen = lines.join("\n");

    // Sanity: confirm the interactive REPL actually ran in the pty. If this
    // fails the harness is broken, not the feature.
    assert!(
        lines.iter().any(|l| l.contains("Sema v")),
        "REPL banner not found — harness did not drive an interactive session.\nScreen:\n{screen}"
    );

    // The sentinel must survive as program OUTPUT — i.e. on a line that is NOT
    // the echoed input (the echoed input line contains the literal `display`).
    // Under the v1.15.0 bug the output line was erased by the prompt repaint
    // and only the input echo remained.
    let survived = lines
        .iter()
        .any(|l| l.contains(sentinel) && !l.contains("display"));
    assert!(
        survived,
        "`display` output was erased by the prompt repaint (REPL display regression).\nScreen:\n{screen}"
    );
}
