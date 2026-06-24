//! SQLite cross-run projection of the JSONL journals (track #2, slice S4).
//!
//! "Store everything by default so the dashboard can show useful things." The frozen
//! `events.jsonl` is the system of record; this module replays it into a queryable
//! `.sema/runs/index.db` so the viewer can answer cross-run questions (which runs,
//! their status/cost/duration) and per-run rollups without re-parsing JSONL each time.
//!
//! Design decisions (adversarially reviewed):
//! - **`run_id` = the directory segment**, NOT the per-event `run_id` field (which is
//!   present ONLY on `run.started`). The dir segment is what the CLI routes, the
//!   path-traversal guard, and `?run=` already key on. The embedded id is stored
//!   separately for display.
//! - **`cost_usd` is NULLABLE** — NULL means "unpriced" (no pricing-table entry), never
//!   `$0`. `SUM` ignores NULLs (and returns NULL iff every row is NULL), so an unpriced
//!   agent is excluded from a cost total rather than counted as free.
//! - **Idempotent**: every typed row is keyed on `(run_id, seq)` with `INSERT OR IGNORE`,
//!   and a per-run byte-offset cursor advances only to the last `\n` — so re-syncing a
//!   growing (or truncated-and-recreated) file never double-counts or corrupts.
//! - **Phase attribution** is `MAX(phases.start_seq) <= event.seq` (the sequential
//!   runtime guarantees one active phase), computed in SQL so it is correct even when a
//!   lazy re-sync starts mid-file.

use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

use rusqlite::{params, Connection};

/// Create the projection schema (idempotent). One cross-run DB; every table keyed by
/// the directory-segment `run_id`.
pub fn init_schema(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        r#"
        PRAGMA journal_mode=WAL;
        PRAGMA synchronous=NORMAL;
        CREATE TABLE IF NOT EXISTS runs (
            run_id          TEXT PRIMARY KEY,
            embedded_run_id TEXT,
            workflow        TEXT,
            code_version    TEXT,
            started_ts      TEXT,
            ended_ts        TEXT,          -- NULL ⇒ still running
            status          TEXT,          -- NULL until run.ended
            args_json       TEXT
        );
        CREATE TABLE IF NOT EXISTS phases (
            run_id    TEXT, start_seq INTEGER, phase TEXT, status TEXT, dur_ms INTEGER,
            PRIMARY KEY (run_id, start_seq)
        );
        CREATE TABLE IF NOT EXISTS agents (
            run_id   TEXT, agent_id TEXT, agent_name TEXT, model TEXT,
            phase_seq INTEGER, status TEXT, dur_ms INTEGER,
            PRIMARY KEY (run_id, agent_id)
        );
        CREATE TABLE IF NOT EXISTS tool_calls (
            run_id TEXT, seq INTEGER, agent_id TEXT, tool_name TEXT, args_json TEXT,
            PRIMARY KEY (run_id, seq)
        );
        CREATE TABLE IF NOT EXISTS checkpoints (
            run_id TEXT, seq INTEGER, phase_seq INTEGER, key TEXT, value_digest TEXT,
            PRIMARY KEY (run_id, seq)
        );
        CREATE TABLE IF NOT EXISTS usage (
            run_id TEXT, seq INTEGER, agent_id TEXT, phase_seq INTEGER,
            input_tokens INTEGER, output_tokens INTEGER, cost_usd REAL, -- nullable!
            PRIMARY KEY (run_id, seq)
        );
        CREATE TABLE IF NOT EXISTS ingest_cursor (
            run_id TEXT PRIMARY KEY, byte_offset INTEGER, started_ts TEXT
        );
        "#,
    )
}

/// Tail-and-replay a run's `events.jsonl` into the typed tables. Cheap on a live run
/// (resumes from the stored byte-offset); fully idempotent (safe to call on every
/// request). `run_id` is the directory segment. Returns the new high-water byte offset.
pub fn sync_run(conn: &Connection, run_dir: &Path, run_id: &str) -> rusqlite::Result<i64> {
    let path = run_dir.join(run_id).join("events.jsonl");
    let Ok(mut file) = std::fs::File::open(&path) else {
        return Ok(0); // nothing to ingest yet
    };
    // SQLite integers are i64; a journal never approaches that size.
    let file_len = file.metadata().map(|m| m.len()).unwrap_or(0) as i64;

    // Prior cursor. If the file shrank below it (truncated / dir recreated), the prior
    // projection is stale: wipe this run's rows and re-ingest from 0.
    let mut offset: i64 = conn
        .query_row(
            "SELECT byte_offset FROM ingest_cursor WHERE run_id=?1",
            params![run_id],
            |r| r.get(0),
        )
        .unwrap_or(0);
    if offset > file_len {
        wipe_run(conn, run_id)?;
        offset = 0;
    }
    if offset >= file_len {
        return Ok(offset); // nothing new
    }

    file.seek(SeekFrom::Start(offset as u64)).ok();
    let mut tail = Vec::new();
    file.read_to_end(&mut tail).ok();
    // Only consume up to the last newline — a trailing partial line waits for the next sync.
    let Some(last_nl) = tail.iter().rposition(|&b| b == b'\n') else {
        return Ok(offset); // no complete line yet
    };
    let complete = &tail[..=last_nl];

    let tx = conn.unchecked_transaction()?;
    for line in complete.split(|&b| b == b'\n') {
        if line.is_empty() {
            continue;
        }
        // A corrupt/truncated line (crash mid-write) is skipped, never fatal.
        let Ok(e) = serde_json::from_slice::<serde_json::Value>(line) else {
            continue;
        };
        project_event(&tx, run_id, &e)?;
    }
    let new_offset = offset + complete.len() as i64;
    let started = run_started_ts(&tx, run_id);
    tx.execute(
        "INSERT INTO ingest_cursor(run_id,byte_offset,started_ts) VALUES(?1,?2,?3)
         ON CONFLICT(run_id) DO UPDATE SET byte_offset=?2",
        params![run_id, new_offset, started],
    )?;
    tx.commit()?;
    Ok(new_offset)
}

fn run_started_ts(conn: &Connection, run_id: &str) -> Option<String> {
    conn.query_row(
        "SELECT started_ts FROM runs WHERE run_id=?1",
        params![run_id],
        |r| r.get(0),
    )
    .ok()
}

fn wipe_run(conn: &Connection, run_id: &str) -> rusqlite::Result<()> {
    for t in [
        "runs",
        "phases",
        "agents",
        "tool_calls",
        "checkpoints",
        "usage",
        "ingest_cursor",
    ] {
        conn.execute(&format!("DELETE FROM {t} WHERE run_id=?1"), params![run_id])?;
    }
    Ok(())
}

fn s(e: &serde_json::Value, k: &str) -> Option<String> {
    e.get(k).and_then(|v| v.as_str()).map(String::from)
}
fn i(e: &serde_json::Value, k: &str) -> Option<i64> {
    e.get(k).and_then(|v| v.as_i64())
}

/// Project one decoded event into the typed tables (idempotent via INSERT OR IGNORE /
/// targeted UPDATE).
fn project_event(conn: &Connection, run_id: &str, e: &serde_json::Value) -> rusqlite::Result<()> {
    let seq = i(e, "seq").unwrap_or(0);
    match e.get("event").and_then(|v| v.as_str()).unwrap_or("") {
        "run.started" => {
            conn.execute(
                "INSERT INTO runs(run_id,embedded_run_id,workflow,code_version,started_ts,args_json)
                 VALUES(?1,?2,?3,?4,?5,?6)
                 ON CONFLICT(run_id) DO UPDATE SET
                   embedded_run_id=?2, workflow=?3, code_version=?4, started_ts=?5, args_json=?6",
                params![
                    run_id,
                    s(e, "run_id"),
                    s(e, "workflow"),
                    s(e, "code_version"),
                    s(e, "ts"),
                    s(e, "args_json"),
                ],
            )?;
        }
        "run.ended" => {
            conn.execute(
                "UPDATE runs SET ended_ts=?2, status=?3 WHERE run_id=?1",
                params![run_id, s(e, "ts"), s(e, "status")],
            )?;
        }
        "phase.started" => {
            conn.execute(
                "INSERT OR IGNORE INTO phases(run_id,start_seq,phase) VALUES(?1,?2,?3)",
                params![run_id, seq, s(e, "phase")],
            )?;
        }
        "phase.ended" => {
            // The latest still-open phase with this label (sequential ⇒ unambiguous).
            conn.execute(
                "UPDATE phases SET status=?3, dur_ms=?4
                 WHERE rowid=(SELECT rowid FROM phases
                              WHERE run_id=?1 AND phase=?2 AND status IS NULL
                              ORDER BY start_seq DESC LIMIT 1)",
                params![run_id, s(e, "phase"), s(e, "status"), i(e, "dur_ms")],
            )?;
        }
        "agent.started" => {
            let phase_seq = open_phase_seq(conn, run_id, seq)?;
            conn.execute(
                "INSERT OR IGNORE INTO agents(run_id,agent_id,agent_name,model,phase_seq)
                 VALUES(?1,?2,?3,?4,?5)",
                params![
                    run_id,
                    s(e, "agent_id"),
                    s(e, "agent_name"),
                    s(e, "model"),
                    phase_seq
                ],
            )?;
        }
        "agent.result" => {
            conn.execute(
                "UPDATE agents SET status=?3, dur_ms=?4,
                   model=COALESCE(NULLIF(?5,''), model)
                 WHERE run_id=?1 AND agent_id=?2",
                params![
                    run_id,
                    s(e, "agent_id"),
                    s(e, "status"),
                    i(e, "dur_ms"),
                    s(e, "model")
                ],
            )?;
        }
        "agent.tool_call" => {
            conn.execute(
                "INSERT OR IGNORE INTO tool_calls(run_id,seq,agent_id,tool_name,args_json)
                 VALUES(?1,?2,?3,?4,?5)",
                params![
                    run_id,
                    seq,
                    s(e, "agent_id"),
                    s(e, "tool_name"),
                    s(e, "args_json")
                ],
            )?;
        }
        "checkpoint" => {
            conn.execute(
                "INSERT OR IGNORE INTO checkpoints(run_id,seq,phase_seq,key,value_digest)
                 VALUES(?1,?2,?3,?4,?5)",
                params![
                    run_id,
                    seq,
                    i(e, "phase_seq"),
                    s(e, "key"),
                    s(e, "value_digest")
                ],
            )?;
        }
        "budget" => {
            // cost_usd bound as NULL when absent — "unpriced", not $0.
            let cost = e.get("cost_usd").and_then(|v| v.as_f64());
            conn.execute(
                "INSERT OR IGNORE INTO usage(run_id,seq,agent_id,phase_seq,input_tokens,output_tokens,cost_usd)
                 VALUES(?1,?2,?3,?4,?5,?6,?7)",
                params![
                    run_id,
                    seq,
                    s(e, "agent_id"),
                    i(e, "phase_seq"),
                    i(e, "input_tokens"),
                    i(e, "output_tokens"),
                    cost,
                ],
            )?;
        }
        _ => {}
    }
    Ok(())
}

/// The phase an event at `seq` belongs to: the greatest `phase.started` seq ≤ `seq`.
fn open_phase_seq(conn: &Connection, run_id: &str, seq: i64) -> rusqlite::Result<Option<i64>> {
    conn.query_row(
        "SELECT MAX(start_seq) FROM phases WHERE run_id=?1 AND start_seq<=?2",
        params![run_id, seq],
        |r| r.get::<_, Option<i64>>(0),
    )
}

/// Replay every run directory under `runs_root` into the DB.
pub fn backfill_all(conn: &Connection, runs_root: &Path) {
    let Ok(entries) = std::fs::read_dir(runs_root) else {
        return;
    };
    for entry in entries.flatten() {
        if entry.path().is_dir() {
            if let Some(id) = entry.file_name().to_str() {
                let _ = sync_run(conn, runs_root, id);
            }
        }
    }
}

/// Cross-run summary list (the dashboard's runs picker): one row per run with its
/// status (running until ended), agent count, total tokens, and total cost (NULL when
/// every contributing agent is unpriced — surfaced as "unpriced", never $0).
pub fn runs_summary(conn: &Connection) -> rusqlite::Result<Vec<serde_json::Value>> {
    let mut stmt = conn.prepare(
        "SELECT r.run_id, r.workflow, COALESCE(r.status,'running') AS status,
                r.started_ts, r.ended_ts,
                (SELECT COUNT(*) FROM agents a WHERE a.run_id=r.run_id) AS agents,
                (SELECT COALESCE(SUM(input_tokens+output_tokens),0) FROM usage u WHERE u.run_id=r.run_id) AS tokens,
                (SELECT SUM(cost_usd) FROM usage u WHERE u.run_id=r.run_id) AS cost
         FROM runs r
         ORDER BY r.started_ts DESC, r.run_id",
    )?;
    let rows = stmt.query_map([], |row| {
        Ok(serde_json::json!({
            "run_id":    row.get::<_, String>(0)?,
            "workflow":  row.get::<_, Option<String>>(1)?,
            "status":    row.get::<_, String>(2)?,
            "started_ts":row.get::<_, Option<String>>(3)?,
            "ended_ts":  row.get::<_, Option<String>>(4)?,
            "agents":    row.get::<_, i64>(5)?,
            "tokens":    row.get::<_, i64>(6)?,
            // None ⇒ unpriced (all contributing rows NULL): the client renders "—".
            "cost_usd":  row.get::<_, Option<f64>>(7)?,
        }))
    })?;
    rows.collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_runs_root() -> std::path::PathBuf {
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/workflow/viewer-runs")
    }

    fn count(conn: &Connection, table: &str) -> i64 {
        conn.query_row(&format!("SELECT COUNT(*) FROM {table}"), [], |r| r.get(0))
            .unwrap()
    }

    #[test]
    fn ingests_audit_auth_fixture_into_typed_tables() {
        let conn = Connection::open_in_memory().unwrap();
        init_schema(&conn).unwrap();
        sync_run(&conn, &fixture_runs_root(), "audit-auth").unwrap();

        // Row counts mirror the fixture (1 run, 4 phases, 4 agents, 5 tool calls,
        // 2 checkpoints, 3 budget rows).
        assert_eq!(count(&conn, "runs"), 1);
        assert_eq!(count(&conn, "phases"), 4);
        assert_eq!(count(&conn, "agents"), 4);
        assert_eq!(count(&conn, "tool_calls"), 5);
        assert_eq!(count(&conn, "checkpoints"), 2);
        assert_eq!(count(&conn, "usage"), 3);

        // run_id is the DIRECTORY segment; the embedded run.started id is display-only.
        let (run_id, embedded, status): (String, String, Option<String>) = conn
            .query_row(
                "SELECT run_id, embedded_run_id, status FROM runs",
                [],
                |r| Ok((r.get(0)?, r.get(1)?, r.get(2)?)),
            )
            .unwrap();
        assert_eq!(run_id, "audit-auth");
        assert_eq!(embedded, "wf_audit_auth_8f3a21");
        assert_eq!(status, None, "live run (no run.ended) ⇒ status NULL");

        // One agent failed.
        let failed: i64 = conn
            .query_row(
                "SELECT COUNT(*) FROM agents WHERE status='failed'",
                [],
                |r| r.get(0),
            )
            .unwrap();
        assert_eq!(failed, 1);

        // NULL-aware cost: SUM ignores the unpriced auditor_2 → 0.0041 + 0.0017.
        let cost: Option<f64> = conn
            .query_row("SELECT SUM(cost_usd) FROM usage", [], |r| r.get(0))
            .unwrap();
        assert!((cost.unwrap() - 0.0058).abs() < 1e-9, "cost = {cost:?}");

        // Phase attribution: all three auditors map to the `audit` phase.
        let audit_seq: i64 = conn
            .query_row(
                "SELECT start_seq FROM phases WHERE phase='audit'",
                [],
                |r| r.get(0),
            )
            .unwrap();
        let auditors_in_audit: i64 = conn
            .query_row(
                "SELECT COUNT(*) FROM agents WHERE agent_name='auditor' AND phase_seq=?1",
                [audit_seq],
                |r| r.get(0),
            )
            .unwrap();
        assert_eq!(auditors_in_audit, 3);

        // Cross-run summary surfaces the live run with the right cost.
        let summary = runs_summary(&conn).unwrap();
        assert_eq!(summary.len(), 1);
        assert_eq!(summary[0]["status"], "running");
        assert_eq!(summary[0]["agents"], 4);
        assert!((summary[0]["cost_usd"].as_f64().unwrap() - 0.0058).abs() < 1e-9);
    }

    #[test]
    fn re_ingesting_is_idempotent() {
        let conn = Connection::open_in_memory().unwrap();
        init_schema(&conn).unwrap();
        let root = fixture_runs_root();
        let tables = [
            "runs",
            "phases",
            "agents",
            "tool_calls",
            "checkpoints",
            "usage",
        ];

        sync_run(&conn, &root, "audit-auth").unwrap();
        let first: Vec<i64> = tables.iter().map(|t| count(&conn, t)).collect();

        // The byte-offset cursor means subsequent syncs process no new lines.
        sync_run(&conn, &root, "audit-auth").unwrap();
        sync_run(&conn, &root, "audit-auth").unwrap();
        let third: Vec<i64> = tables.iter().map(|t| count(&conn, t)).collect();

        assert_eq!(first, third, "re-ingest must not change row counts");
    }
}
