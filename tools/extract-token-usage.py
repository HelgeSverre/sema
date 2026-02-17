#!/usr/bin/env python3
"""Extract per-day token usage data from Claude Code and Amp for the sema-lisp project.

Usage: python3 tools/extract-token-usage.py
Output: tools/token-data.json
"""

import json
import os
import glob
from datetime import datetime, timezone
from collections import defaultdict

OUTPUT_PATH = os.path.join(os.path.dirname(__file__), "token-data.json")

CLAUDE_CODE_DIRS = [
    "~/.claude/projects/-Users-helge-code-sema-lisp",
    "~/.claude/projects/-Users-helge-code-lisp",
    "~/.claude/projects/-Users-helge-code-lisp-examples",
    "~/.claude/projects/-Users-helge-code-lisp-website",
    "~/.claude/projects/-Users-helge-conductor-workspaces-lisp-montevideo",
    "~/.claude/projects/-Users-helge-conductor-workspaces-lisp-san-jose",
    "~/.claude/projects/-Users-helge-conductor-workspaces-lisp-tripoli",
]

AMP_THREADS_DIR = "~/.local/share/amp/threads"
AMP_PROJECT_KEYWORDS = ["lisp", "sema-lisp", "sema_lisp"]


def parse_timestamp(ts):
    if ts is None:
        return None
    if isinstance(ts, (int, float)):
        return datetime.fromtimestamp(ts / 1000, tz=timezone.utc)
    if isinstance(ts, str):
        try:
            return datetime.fromisoformat(ts.replace("Z", "+00:00"))
        except ValueError:
            pass
    return None


def safe_int(val):
    return int(val) if val else 0


def get_first_user_message(filepath):
    try:
        with open(filepath) as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    obj = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if obj.get("type") == "user":
                    msg = obj.get("message", {})
                    if isinstance(msg, dict):
                        content = msg.get("content", "")
                        if isinstance(content, list):
                            for c in content:
                                if isinstance(c, dict) and c.get("type") == "text":
                                    text = c["text"].strip()
                                    return text[:120] + "..." if len(text) > 120 else text
                        elif isinstance(content, str):
                            text = content.strip()
                            return text[:120] + "..." if len(text) > 120 else text
    except OSError:
        pass
    return os.path.basename(filepath)


def extract_claude_code():
    daily = defaultdict(lambda: {
        "input": 0, "output": 0, "cache_read": 0, "cache_write": 0,
        "messages": 0, "sessions": 0
    })
    by_model = defaultdict(lambda: {
        "input": 0, "output": 0, "cache_read": 0, "cache_write": 0, "messages": 0
    })
    total = {"input": 0, "output": 0, "cache_read": 0, "cache_write": 0, "messages": 0, "sessions": 0}
    threads = []
    sessions_by_date = defaultdict(set)

    for project_dir in CLAUDE_CODE_DIRS:
        project_dir = os.path.expanduser(project_dir)
        if not os.path.isdir(project_dir):
            continue

        for filepath in glob.glob(os.path.join(project_dir, "*.jsonl")):
            file_mtime = datetime.fromtimestamp(os.path.getmtime(filepath), tz=timezone.utc)
            session_name = os.path.basename(filepath)
            tt = {"input": 0, "output": 0, "cache_read": 0, "cache_write": 0, "messages": 0}
            thread_first_date = None

            try:
                with open(filepath) as f:
                    for line in f:
                        line = line.strip()
                        if not line:
                            continue
                        try:
                            obj = json.loads(line)
                        except json.JSONDecodeError:
                            continue

                        if obj.get("type") != "assistant":
                            continue
                        msg = obj.get("message", {})
                        if not isinstance(msg, dict):
                            continue
                        usage = msg.get("usage")
                        if not usage:
                            continue

                        model = msg.get("model", "unknown")
                        inp = safe_int(usage.get("input_tokens"))
                        out = safe_int(usage.get("output_tokens"))
                        cr = safe_int(usage.get("cache_read_input_tokens"))
                        cw = safe_int(usage.get("cache_creation_input_tokens"))

                        ts = parse_timestamp(obj.get("timestamp")) or file_mtime
                        date_str = ts.strftime("%Y-%m-%d")
                        if thread_first_date is None:
                            thread_first_date = date_str

                        sessions_by_date[date_str].add(session_name)

                        for store in [daily[date_str], by_model[model], total, tt]:
                            store["input"] += inp
                            store["output"] += out
                            store["cache_read"] += cr
                            store["cache_write"] += cw
                            store["messages"] += 1

            except OSError as e:
                print(f"Error reading {filepath}: {e}")
                continue

            if tt["messages"] > 0:
                title = get_first_user_message(filepath)
                threads.append({
                    "title": title,
                    "date": thread_first_date or file_mtime.strftime("%Y-%m-%d"),
                    "messages": tt["messages"],
                    "input": tt["input"],
                    "output": tt["output"],
                    "cache_read": tt["cache_read"],
                    "cache_write": tt["cache_write"],
                })

    for date_str, session_set in sessions_by_date.items():
        daily[date_str]["sessions"] = len(session_set)
    total["sessions"] = len(set().union(*sessions_by_date.values())) if sessions_by_date else 0

    daily_list = sorted([{"date": k, **v} for k, v in daily.items()], key=lambda x: x["date"])
    threads.sort(key=lambda x: x["cache_read"] + x["cache_write"] + x["output"] + x["input"], reverse=True)

    return {"daily": daily_list, "by_model": dict(by_model), "total": total, "threads": threads[:30]}


def extract_amp():
    threads_dir = os.path.expanduser(AMP_THREADS_DIR)
    if not os.path.isdir(threads_dir):
        return {"daily": [], "by_model": {}, "total": {"input": 0, "output": 0, "cache_read": 0, "cache_write": 0, "messages": 0, "sessions": 0}, "threads": []}

    daily = defaultdict(lambda: {
        "input": 0, "output": 0, "cache_read": 0, "cache_write": 0,
        "messages": 0, "sessions": 0
    })
    by_model = defaultdict(lambda: {
        "input": 0, "output": 0, "cache_read": 0, "cache_write": 0, "messages": 0
    })
    total = {"input": 0, "output": 0, "cache_read": 0, "cache_write": 0, "messages": 0, "sessions": 0}
    threads = []
    sessions_by_date = defaultdict(set)

    for filepath in glob.glob(os.path.join(threads_dir, "T-*.json")):
        try:
            with open(filepath) as f:
                data = json.load(f)
        except (OSError, json.JSONDecodeError):
            continue

        is_lisp = False
        for tree in data.get("env", {}).get("initial", {}).get("trees", []):
            uri = tree.get("uri", "")
            name = tree.get("displayName", "")
            if any(p in uri.lower() or p in name.lower() for p in AMP_PROJECT_KEYWORDS):
                is_lisp = True
                break
        if not is_lisp:
            continue

        thread_ts = parse_timestamp(data.get("created"))
        if thread_ts is None:
            thread_ts = datetime.fromtimestamp(os.path.getmtime(filepath), tz=timezone.utc)
        date_str = thread_ts.strftime("%Y-%m-%d")

        title = data.get("title", os.path.basename(filepath))
        session_name = os.path.basename(filepath)
        tt = {"input": 0, "output": 0, "cache_read": 0, "cache_write": 0, "messages": 0}

        for msg in data.get("messages", []):
            if msg.get("role") != "assistant":
                continue
            usage = msg.get("usage")
            if not usage or not isinstance(usage, dict):
                continue

            model = usage.get("model", "unknown")
            inp = safe_int(usage.get("inputTokens"))
            out = safe_int(usage.get("outputTokens"))
            cr = safe_int(usage.get("cacheReadInputTokens"))
            cw = safe_int(usage.get("cacheCreationInputTokens"))

            sessions_by_date[date_str].add(session_name)

            for store in [daily[date_str], by_model[model], total, tt]:
                store["input"] += inp
                store["output"] += out
                store["cache_read"] += cr
                store["cache_write"] += cw
                store["messages"] += 1

        if tt["messages"] > 0:
            threads.append({
                "title": title,
                "date": date_str,
                "messages": tt["messages"],
                "input": tt["input"],
                "output": tt["output"],
                "cache_read": tt["cache_read"],
                "cache_write": tt["cache_write"],
            })

    for date_str, session_set in sessions_by_date.items():
        daily[date_str]["sessions"] = len(session_set)
    total["sessions"] = len(set().union(*sessions_by_date.values())) if sessions_by_date else 0

    daily_list = sorted([{"date": k, **v} for k, v in daily.items()], key=lambda x: x["date"])
    threads.sort(key=lambda x: x["cache_read"] + x["cache_write"] + x["output"] + x["input"], reverse=True)

    return {"daily": daily_list, "by_model": dict(by_model), "total": total, "threads": threads[:30]}


def main():
    print("Extracting Claude Code data...")
    cc = extract_claude_code()
    print(f"  {len(cc['daily'])} days, {cc['total']['messages']} messages, {len(cc['threads'])} threads")

    print("Extracting Amp data...")
    amp = extract_amp()
    print(f"  {len(amp['daily'])} days, {amp['total']['messages']} messages, {len(amp['threads'])} threads")

    with open(OUTPUT_PATH, "w") as f:
        json.dump({"claude_code": cc, "amp": amp}, f, indent=2)

    print(f"\nWritten to {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
