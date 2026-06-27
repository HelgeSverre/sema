#!/usr/bin/env python3
"""
Generate tool-repair traces for SFT training.

Each trace captures the agentic loop:
  prompt -> draft code -> eval_code error -> corrected code -> final answer

This trains the model to READ errors and FIX code, not just produce
first-draft answers. This is the behavior Ornith is good at and that
the previous RFT run destroyed.

Uses a frontier model with eval_code + docs_search tools to solve tasks,
then keeps only traces that:
  1. Contain at least one error->repair cycle
  2. End with a correct answer (verified by the Sema VM)

Usage:
  python3 generate_repair_traces.py \
    --sema-path ../target/debug/sema \
    --base-url https://api.fireworks.ai/inference/v1 \
    --api-key $FIREWORKS_API_KEY \
    --llm-model accounts/fireworks/models/glm-5p2 \
    --tasks data/repair_tasks.jsonl \
    --output data/sft/repair_traces.jsonl
"""

import argparse
import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path

import httpx

SCRIPT_DIR = Path(__file__).parent
REPO_ROOT = SCRIPT_DIR.parent
SEMA_BINARY = str(REPO_ROOT / "target" / "debug" / "sema")
MAX_TOOL_ROUNDS = 6

TOOL_DEFS = [
    {"type": "function", "function": {
        "name": "eval_code",
        "description": "Evaluate Sema code and return the result. Use this to test your code before returning it. Returns the evaluated value as a string, or an error message.",
        "parameters": {"type": "object", "properties": {
            "code": {"type": "string", "description": "The Sema code to evaluate"}}, "required": ["code"]}}},
    {"type": "function", "function": {
        "name": "docs_search",
        "description": "Search Sema documentation semantically. Returns relevant doc entries with function names, descriptions, and code examples.",
        "parameters": {"type": "object", "properties": {
            "query": {"type": "string", "description": "What you're looking for"}}, "required": ["query"]}}},
]

SYSTEM_PROMPT_AGENT = (
    "You are a Sema coding agent. Use eval_code to test code. "
    "When you get an error, read it carefully and fix the code. "
    "Output Sema code directly — no Markdown fences, no backticks."
)


def call_model(base_url, api_key, model, messages, tools=None, max_tokens=2048, temperature=0.6):
    url = f"{base_url.rstrip('/')}/chat/completions"
    payload = {"model": model, "messages": messages, "max_tokens": max_tokens,
               "temperature": temperature, "top_p": 0.95}
    if tools:
        payload["tools"] = tools
        payload["tool_choice"] = "auto"
    headers = {"Content-Type": "application/json",
               "Authorization": f"Bearer {api_key}"}
    try:
        with httpx.Client(timeout=180) as c:
            r = c.post(url, json=payload, headers=headers)
            return r.json()
    except Exception as e:
        return {"error": str(e)}


def execute_eval_code(code):
    try:
        r = subprocess.run(
            [SEMA_BINARY, "eval", "--expr", code, "--json", "--timeout", "5000", "--no-llm"],
            capture_output=True, text=True, timeout=15)
        if r.returncode == 0:
            d = json.loads(r.stdout)
            if d.get("ok"):
                val = d.get("value", "")
                stdout = d.get("stdout", "")
                return f"Result: {val}" + (f"\nStdout: {stdout}" if stdout else "")
            err = d.get("error", {})
            return f"Error: {err.get('message', 'unknown')}"
        return f"Error: {r.stderr.strip()[:300]}"
    except Exception as e:
        return f"Error: {e}"


def execute_docs_search(query):
    eq = query.replace('"', '\\"')
    code = (
        '(begin (llm/auto-configure) (vector-store/open "docs" "/tmp/sema-docs-rag.vec") '
        '(let* ((qv (llm/embed "' + eq + '")) '
        '(c (vector-store/search "docs" qv 10)) '
        '(ct (map (lambda (x) (:text (:metadata x))) c)) '
        '(r (llm/rerank "' + eq + '" ct {:top-k 5}))) '
        '(string/join (map (lambda (x) (let* ((i (:index x)) (m (nth c i))) '
        '(string-append "### " (str (:name (:metadata m))) "\\n" (str (:text (:metadata m)))))) r) "\\n---\\n")))'
    )
    try:
        r = subprocess.run(
            [SEMA_BINARY, "eval", "--expr", code, "--json", "--timeout", "30000"],
            capture_output=True, text=True, timeout=60, cwd=str(REPO_ROOT))
        if r.returncode == 0:
            d = json.loads(r.stdout)
            return d.get("value", "No results") or "No results"
        return f"Search error: {r.stderr.strip()[:200]}"
    except Exception as e:
        return f"Search error: {e}"


def execute_tool(name, args):
    if name == "eval_code":
        return execute_eval_code(args.get("code", ""))
    if name == "docs_search":
        return execute_docs_search(args.get("query", ""))
    return f"Unknown tool: {name}"


def extract_sema_code(completion):
    for p in [r'```(?:sema|lisp|scheme|clj)?\s*\n(.*?)```', r'```(?:sema|lisp|scheme|clj)?\s*(.*?)```']:
        m = re.findall(p, completion, re.DOTALL)
        if m:
            return m[0].strip()
    s = completion.strip()
    if s and s[0] in '([{;\'`#':
        return s
    for i, l in enumerate(s.split('\n')):
        if l.strip().startswith('('):
            return '\n'.join(s.split('\n')[i:]).strip()
    return s if s else None


def sema_eval(sema_path, code, timeout=10):
    try:
        r = subprocess.run(
            [sema_path, "eval", "--expr", code, "--json", "--timeout", "5000", "--no-llm"],
            capture_output=True, text=True, timeout=timeout)
        if r.returncode == 0:
            return json.loads(r.stdout)
        return {"ok": False, "error": {"message": r.stderr.strip()}}
    except Exception as e:
        return {"ok": False, "error": {"message": str(e)}}


def verify_solution(sema_path, task, final_code):
    """Check if the final code passes the task's test."""
    grader_type = task.get("grader", "functional")
    expected = str(task.get("expected", "")).strip() if task.get("expected") is not None else None

    if grader_type == "eval_match":
        return expected is not None and extract_sema_code(final_code) is not None

    code = extract_sema_code(final_code)
    if not code:
        return False
    test_code = task.get("test_code")
    full_code = f"(begin {code}\n {test_code})" if test_code else code
    result = sema_eval(sema_path, full_code)
    if not result.get("ok"):
        return False
    if expected is None:
        return True
    actual = str(result.get("value", "")).strip()
    if actual == expected:
        return True
    return re.sub(r'\s+', ' ', actual) == re.sub(r'\s+', ' ', expected)


def generate_repair_trace(base_url, api_key, model, task, sema_path):
    """Run a task with tools and capture the full conversation as an SFT trace."""
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT_AGENT},
        {"role": "user", "content": task["prompt"]},
    ]

    trace_messages = [
        {"role": "system", "content": SYSTEM_PROMPT_AGENT},
        {"role": "user", "content": task["prompt"]},
    ]

    had_error = False
    had_repair = False
    tool_calls_made = 0

    for round_num in range(MAX_TOOL_ROUNDS):
        resp = call_model(base_url, api_key, model, messages, tools=TOOL_DEFS)
        if "error" in resp:
            return None

        choice = resp.get("choices", [{}])[0]
        msg = choice.get("message", {})
        content = msg.get("content", "") or ""
        tool_calls = msg.get("tool_calls", [])

        if not tool_calls:
            # Final answer
            trace_messages.append({"role": "assistant", "content": content})
            # Check if this is a correct solution
            if had_error and had_repair:
                if verify_solution(sema_path, task, content):
                    return {"messages": trace_messages, "had_error": True,
                            "had_repair": True, "tool_calls": tool_calls_made,
                            "source": f"repair_trace:{task['id']}"}
            return None

        # Record assistant message with tool calls
        trace_msg = {"role": "assistant", "content": content}
        if tool_calls:
            trace_msg["tool_calls"] = [
                {"name": tc.get("function", {}).get("name", ""),
                 "arguments": tc.get("function", {}).get("arguments", "{}")}
                for tc in tool_calls
            ]
        trace_messages.append(trace_msg)
        messages.append(msg)

        for tc in tool_calls:
            func = tc.get("function", {})
            tool_name = func.get("name", "")
            try:
                args = json.loads(func.get("arguments", "{}"))
            except json.JSONDecodeError:
                args = {}

            tool_result = execute_tool(tool_name, args)
            tool_calls_made += 1

            # Check if this was an error
            if tool_name == "eval_code" and tool_result.startswith("Error:"):
                had_error = True
            elif tool_name == "eval_code" and had_error and tool_result.startswith("Result:"):
                had_repair = True

            messages.append({
                "role": "tool",
                "tool_call_id": tc.get("id", ""),
                "content": tool_result,
            })
            trace_messages.append({
                "role": "tool",
                "name": tool_name,
                "content": tool_result,
            })

        if round_num == MAX_TOOL_ROUNDS - 2:
            messages.append({
                "role": "user",
                "content": "Give your final answer now. Do not call more tools.",
            })

    # Final answer without tools
    resp = call_model(base_url, api_key, model, messages)
    if "error" in resp:
        return None
    content = resp.get("choices", [{}])[0].get("message", {}).get("content", "") or ""
    trace_messages.append({"role": "assistant", "content": content})

    if had_error and had_repair and verify_solution(sema_path, task, content):
        return {"messages": trace_messages, "had_error": True,
                "had_repair": True, "tool_calls": tool_calls_made,
                "source": f"repair_trace:{task['id']}"}

    return None


def main():
    parser = argparse.ArgumentParser(description="Generate tool-repair traces for SFT")
    parser.add_argument("--sema-path", default=None)
    parser.add_argument("--base-url", default=None)
    parser.add_argument("--api-key", default=None)
    parser.add_argument("--llm-model", default="accounts/fireworks/models/glm-5p2")
    parser.add_argument("--tasks", default="data/benchmark_tasks.jsonl",
                        help="Tasks to generate traces from (NOT for training — these are eval tasks)")
    parser.add_argument("--output", default="data/sft/repair_traces.jsonl")
    parser.add_argument("--limit", type=int, default=None)
    parser.add_argument("--max-traces", type=int, default=50, help="Max successful traces to keep")
    args = parser.parse_args()

    base_url = args.base_url or os.environ.get("OPENAI_BASE_URL", "https://api.fireworks.ai/inference/v1")
    api_key = args.api_key or os.environ.get("FIREWORKS_API_KEY") or os.environ.get("OPENAI_API_KEY")
    if not api_key:
        print("ERROR: No API key found", file=sys.stderr)
        sys.exit(1)

    sema_path = args.sema_path or SEMA_BINARY
    if not Path(sema_path).exists():
        print(f"ERROR: sema not found at {sema_path}", file=sys.stderr)
        sys.exit(1)

    tasks = []
    with (SCRIPT_DIR / args.tasks).open() as f:
        for line in f:
            if line.strip():
                tasks.append(json.loads(line))
    if args.limit:
        tasks = tasks[:args.limit]

    out_path = SCRIPT_DIR / args.output
    out_path.parent.mkdir(parents=True, exist_ok=True)

    successful = 0
    failed = 0

    with out_path.open("w") as f:
        for i, task in enumerate(tasks):
            print(f"\n[{i+1}/{len(tasks)}] {task['id']} ({task['category']})")
            t0 = time.time()
            try:
                trace = generate_repair_trace(base_url, api_key, args.llm_model, task, sema_path)
            except Exception as e:
                trace = None
                print(f"  Exception: {e}")
            lat = time.time() - t0

            if trace:
                f.write(json.dumps(trace) + "\n")
                f.flush()
                successful += 1
                print(f"  -> SUCCESS (tools={trace['tool_calls']}, {lat:.1f}s)")
            else:
                failed += 1
                print(f"  -> SKIP (no error->repair cycle, {lat:.1f}s)")

            if successful >= args.max_traces:
                print(f"\nReached max traces ({args.max_traces})")
                break

    print(f"\n{'='*60}")
    print(f"Repair traces: {successful} successful, {failed} skipped")
    print(f"Output: {out_path}")


if __name__ == "__main__":
    main()
