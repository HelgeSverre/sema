#!/usr/bin/env python3
"""
Generate SFT dataset for Sema code generation.

The previous RFT experiment trained on eval-answer pairs:
  "Evaluate this Sema expression: (+ 1 2)" -> "3"

This generator produces code-generation pairs instead:
  "Write a Sema function that doubles every number in a list" ->
  "(define (double-all xs) (map (fn (x) (* x 2)) xs))"

Sources:
  1. Example .sema files (138) -> function-level snippets with comment descriptions
  2. Doc entries (829) -> API usage examples with natural language queries
  3. Eval test cases (1037) -> code-gen pairs with template descriptions
  4. Negative/correction examples -> Clojure/Scheme anti-patterns

All generated code is filtered through the Sema VM for correctness.

Usage:
  python3 generate_sft_dataset.py --sema-path ../target/debug/sema --output data/sft/

  # With a frontier model for better NL descriptions
  python3 generate_sft_dataset.py --sema-path ../target/debug/sema \
    --use-llm --base-url https://api.fireworks.ai/inference/v1 \
    --api-key $FIREWORKS_API_KEY --llm-model accounts/fireworks/models/glm-5p2 \
    --output data/sft/
"""

import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path

# ─── Config ───────────────────────────────────────────────────────────────────

SCRIPT_DIR = Path(__file__).parent
REPO_ROOT = SCRIPT_DIR.parent

EXAMPLE_DIRS = [
    REPO_ROOT / "examples",
    REPO_ROOT / "playground" / "examples",
]

DOC_ENTRIES_DIR = REPO_ROOT / "crates" / "sema-docs" / "entries"

EVAL_TEST_FILES = [
    "crates/sema/tests/eval_test.rs",
    "crates/sema/tests/eval_core_test.rs",
    "crates/sema/tests/eval_collections_test.rs",
    "crates/sema/tests/eval_stdlib_test.rs",
    "crates/sema/tests/eval_map_test.rs",
    "crates/sema/tests/eval_data_test.rs",
    "crates/sema/tests/eval_types_test.rs",
    "crates/sema/tests/eval_ergonomic_test.rs",
]

SYSTEM_PROMPT_CODEGEN = (
    "You write valid Sema code. Output code only unless asked to explain. "
    "Do not use Markdown fences or backticks."
)

# ─── Sema VM Filter ───────────────────────────────────────────────────────────

def sema_eval(sema_path, code, timeout=10):
    try:
        r = subprocess.run(
            [sema_path, "eval", "--expr", code, "--json", "--timeout", "5000", "--no-llm"],
            capture_output=True, text=True, timeout=timeout,
        )
        if r.returncode == 0:
            return json.loads(r.stdout)
        return {"ok": False, "error": {"message": r.stderr.strip()}}
    except Exception as e:
        return {"ok": False, "error": {"message": str(e)}}


def vm_filter(sema_path, code):
    """Return True if the code parses and runs without error."""
    if not sema_path:
        return True  # skip filtering if no binary
    result = sema_eval(sema_path, code)
    return result.get("ok", False)


# ─── LLM Description Generator ────────────────────────────────────────────────

def generate_description_with_llm(base_url, api_key, model, code):
    """Use a frontier model to generate a natural language description of Sema code."""
    url = f"{base_url.rstrip('/')}/chat/completions"
    payload = {
        "model": model,
        "messages": [
            {"role": "system", "content": "Describe what this Sema (Lisp) code does in one sentence. Start with a verb. Do not mention Sema or Lisp. Example: 'Reverse a list using foldl.'"},
            {"role": "user", "content": code},
        ],
        "max_tokens": 100,
        "temperature": 0.0,
    }
    import httpx
    try:
        with httpx.Client(timeout=30) as client:
            resp = client.post(url, json=payload, headers={
                "Content-Type": "application/json",
                "Authorization": f"Bearer {api_key}",
            })
            result = resp.json()
            return result.get("choices", [{}])[0].get("message", {}).get("content", "").strip()
    except Exception:
        return ""


# ─── Source 1: Example .sema files ────────────────────────────────────────────

def extract_top_level_forms(source):
    """Split Sema source into top-level forms, preserving preceding comments."""
    forms = []
    current_comment = []
    depth = 0
    buf = []
    in_string = False
    in_comment = False
    i = 0

    while i < len(source):
        c = source[i]

        if in_comment:
            if c == '\n':
                in_comment = False
            i += 1
            continue

        if not in_string and c == ';':
            # Comment line
            rest = source[i:]
            end = rest.find('\n')
            if end == -1:
                end = len(rest)
            line = rest[:end].strip()
            current_comment.append(line.lstrip(';').strip())
            i += end
            continue

        if not in_string and c in '"':
            in_string = not in_string
            buf.append(c)
            i += 1
            continue

        if not in_string and c == '\\' and i + 1 < len(source):
            buf.append(c)
            buf.append(source[i + 1])
            i += 2
            continue

        if not in_string and c == '(':
            if depth == 0 and buf and buf[-1] != '\n':
                pass
            depth += 1
            buf.append(c)
            i += 1
            continue

        if not in_string and c == ')':
            depth -= 1
            buf.append(c)
            if depth == 0:
                form_text = ''.join(buf).strip()
                if form_text:
                    comment = ' '.join(c for c in current_comment if c)
                    forms.append((comment, form_text))
                buf = []
                current_comment = []
            i += 1
            continue

        if depth > 0:
            buf.append(c)
        i += 1

    return forms


def parse_example_files(sema_path, use_llm=False, llm_config=None):
    """Extract code-gen pairs from example .sema files."""
    examples = []

    for example_dir in EXAMPLE_DIRS:
        if not example_dir.exists():
            continue
        for sema_file in sorted(example_dir.rglob("*.sema")):
            if "benchmark" in str(sema_file):
                continue
            source = sema_file.read_text()

            # Extract file-level description from first comment
            first_line = source.split('\n')[0]
            file_desc = ""
            if first_line.startswith(';;'):
                file_desc = first_line.lstrip(';').strip()
                if file_desc.startswith(' '):
                    file_desc = file_desc.strip()
                # Remove "filename.sema — " prefix
                if '—' in file_desc:
                    file_desc = file_desc.split('—', 1)[1].strip()
                elif '-' in file_desc and not file_desc.startswith('-'):
                    parts = file_desc.split('-', 1)
                    if len(parts) == 2:
                        file_desc = parts[1].strip()

            # Extract individual top-level forms with their comments
            forms = extract_top_level_forms(source)
            for comment, code in forms:
                if len(code) < 10 or len(code) > 2000:
                    continue
                # Skip defines that are just constants
                if re.match(r'^\(define\s+\S+\s+\d+\)$', code):
                    continue
                # Skip println/display-only lines
                if re.match(r'^\((println|display|print)\s', code):
                    continue

                # Use the comment as the prompt, or the file description
                if comment:
                    prompt = f"Write Sema code: {comment}"
                elif file_desc:
                    prompt = f"Write Sema code for: {file_desc}"
                else:
                    # Use LLM to generate description if available
                    if use_llm and llm_config:
                        desc = generate_description_with_llm(
                            llm_config["base_url"], llm_config["api_key"],
                            llm_config["model"], code)
                        if desc:
                            prompt = f"Write Sema code that: {desc}"
                        else:
                            continue
                    else:
                        continue

                # Filter through VM
                if sema_path and not vm_filter(sema_path, code):
                    continue

                examples.append({
                    "messages": [
                        {"role": "system", "content": SYSTEM_PROMPT_CODEGEN},
                        {"role": "user", "content": prompt},
                        {"role": "assistant", "content": code},
                    ],
                    "source": f"example:{sema_file.name}",
                })

    return examples


# ─── Source 2: Doc entries ────────────────────────────────────────────────────

def parse_doc_entries(sema_path):
    """Extract code-gen pairs from Sema doc entries."""
    examples = []

    if not DOC_ENTRIES_DIR.exists():
        return examples

    for doc_file in sorted(DOC_ENTRIES_DIR.rglob("*.md")):
        text = doc_file.read_text()

        # Parse YAML frontmatter
        name = ""
        module = ""
        syntax = ""
        if text.startswith("---"):
            end = text.find("---", 3)
            if end != -1:
                frontmatter = text[3:end]
                for line in frontmatter.split('\n'):
                    if line.strip().startswith('name:'):
                        name = line.split(':', 1)[1].strip().strip('"')
                    elif line.strip().startswith('module:'):
                        module = line.split(':', 1)[1].strip().strip('"')
                    elif line.strip().startswith('syntax:'):
                        syntax = line.split(':', 1)[1].strip().strip('"')

        # Extract code examples from ```sema blocks
        code_blocks = re.findall(r'```sema\n(.*?)```', text, re.DOTALL)

        for code in code_blocks:
            code = code.strip()
            if not code or len(code) < 5 or len(code) > 1500:
                continue

            # Strip trailing comment lines (e.g. ";; => result" or "; => result")
            lines = code.split('\n')
            cleaned = []
            for line in lines:
                stripped = line.strip()
                if stripped.startswith(';') and '=>' in stripped:
                    continue
                cleaned.append(line)
            code = '\n'.join(cleaned).strip()

            if not code or len(code) < 5:
                continue

            # Skip examples that are just comments
            if code.startswith(';;') and '\n' not in code:
                continue

            # Generate prompt from doc metadata
            if syntax:
                prompt = f"Show me how to use {name} in Sema. Syntax: {syntax}"
            else:
                prompt = f"Show me an example of {name} in Sema."

            # Filter through VM
            if sema_path and not vm_filter(sema_path, code):
                continue

            examples.append({
                "messages": [
                    {"role": "system", "content": SYSTEM_PROMPT_CODEGEN},
                    {"role": "user", "content": prompt},
                    {"role": "assistant", "content": code},
                ],
                "source": f"doc:{module}/{name}",
            })

    return examples


# ─── Source 3: Eval test cases ────────────────────────────────────────────────

def strip_rust_comments(text):
    lines = text.split('\n')
    result = []
    for line in lines:
        in_string = False
        i = 0
        while i < len(line):
            c = line[i]
            if c == '"' and (i == 0 or line[i-1] != '\\'):
                in_string = not in_string
            elif c == '/' and i + 1 < len(line) and line[i+1] == '/' and not in_string:
                break
            i += 1
        result.append(line[:i] if i < len(line) else line)
    return '\n'.join(result)


def extract_eval_tests(test_file_path):
    """Extract (test_name, input_code, expected_value) from eval_tests! macros."""
    if not test_file_path.exists():
        return []

    source = test_file_path.read_text()
    source = strip_rust_comments(source)

    pairs = []
    # Pattern 1: raw strings — name: r#"code"# => Value::type(expected)
    # The code can contain " but the terminator is "#
    raw_pattern = r'(\w+):\s*r#"(.*?)"#\s*=>\s*Value::(\w+)\(([^)]*)\)'
    for m in re.finditer(raw_pattern, source, re.DOTALL):
        name, code, val_type, val_content = m.groups()
        code = code.strip()
        if not code or len(code) > 500:
            continue
        expected = val_content.strip().strip('"')
        pairs.append({"name": name, "code": code,
                       "expected_type": val_type, "expected": expected})

    # Pattern 2: regular strings — name: "code" => Value::type(expected)
    # Handle escaped quotes inside the string
    str_pattern = r'(\w+):\s*"((?:[^"\\]|\\.)*)"\s*=>\s*Value::(\w+)\(([^)]*)\)'
    for m in re.finditer(str_pattern, source, re.DOTALL):
        name, code, val_type, val_content = m.groups()
        # Unescape
        code = code.replace('\\"', '"').replace("\\'", "'").replace('\\\\', '\\')
        code = code.strip()
        if not code or len(code) > 500:
            continue
        # Skip if already captured by raw pattern
        if any(p["name"] == name for p in pairs):
            continue
        expected = val_content.strip().strip('"')
        pairs.append({"name": name, "code": code,
                       "expected_type": val_type, "expected": expected})

    return pairs


def snake_to_description(name):
    """Convert a snake_case test name to a human-readable description."""
    words = name.split('_')
    return ' '.join(words).strip()


def generate_eval_description(code, expected, test_name=""):
    """Generate a template natural language description for an eval pair."""
    code_stripped = code.strip()

    # Use the test name as a primary hint (it's usually descriptive)
    if test_name and len(test_name) > 2:
        desc = snake_to_description(test_name)
        # Try to infer the category from the code
        if code_stripped.startswith('(define '):
            return f"Write Sema code: {desc}"
        return f"Write a Sema expression: {desc}"

    # Fall back to structural analysis
    arith_match = re.match(r'^\(([+\-*/])\s+(.+)\)$', code_stripped)
    if arith_match:
        op = arith_match.group(1)
        args = arith_match.group(2).split()
        op_names = {'+': 'add', '-': 'subtract', '*': 'multiply', '/': 'divide'}
        if len(args) == 2:
            return f"Write Sema code to {op_names.get(op, op)} {args[0]} and {args[1]}"

    if code_stripped.startswith('(map '):
        return "Write Sema code that maps over a list"
    if code_stripped.startswith('(filter '):
        return "Write Sema code that filters a list"
    if code_stripped.startswith('(foldl '):
        return "Write Sema code that folds a list from the left"
    if code_stripped.startswith('(reduce '):
        return "Write Sema code that reduces a list"

    if 'string/' in code_stripped:
        return "Write Sema code using string functions"

    if code_stripped.startswith('(reverse '):
        return "Write Sema code to reverse a list"
    if code_stripped.startswith('(sort '):
        return "Write Sema code to sort a list"
    if code_stripped.startswith('(take '):
        return "Write Sema code to take elements from a list"
    if code_stripped.startswith('(drop '):
        return "Write Sema code to drop elements from a list"

    define_match = re.match(r'^\(define\s+\((\w+)', code_stripped)
    if define_match:
        func_name = define_match.group(1)
        return f"Write a Sema function called {func_name}"

    if code_stripped.startswith('(let '):
        return "Write Sema code using let bindings"
    if code_stripped.startswith('(match '):
        return "Write Sema code using pattern matching"
    if code_stripped.startswith('(cond '):
        return "Write Sema code using cond"

    return "Write a Sema expression"


def parse_eval_tests(sema_path, use_llm=False, llm_config=None):
    """Extract code-gen pairs from eval test cases."""
    examples = []

    for test_file_rel in EVAL_TEST_FILES:
        test_file = REPO_ROOT / test_file_rel
        pairs = extract_eval_tests(test_file)
        for pair in pairs:
            code = pair["code"]
            expected = pair["expected"]

            # Filter through VM
            if sema_path:
                result = sema_eval(sema_path, code)
                if not result.get("ok"):
                    continue

            # Generate description
            if use_llm and llm_config:
                desc = generate_description_with_llm(
                    llm_config["base_url"], llm_config["api_key"],
                    llm_config["model"], code)
                prompt = f"Write Sema code that: {desc}" if desc else generate_eval_description(code, expected, pair["name"])
            else:
                prompt = generate_eval_description(code, expected, pair["name"])

            examples.append({
                "messages": [
                    {"role": "system", "content": SYSTEM_PROMPT_CODEGEN},
                    {"role": "user", "content": prompt},
                    {"role": "assistant", "content": code},
                ],
                "source": f"eval_test:{pair['name']}",
            })

    return examples


# ─── Source 4: Negative/correction examples ───────────────────────────────────

NEGATIVE_EXAMPLES = [
    {
        "prompt": "Write a Sema let binding for x=1, y=2",
        "bad": "(let [x 1 y 2] (+ x y))",
        "good": "(let ((x 1) (y 2)) (+ x y))",
        "explanation": "Sema uses parenthesized binding lists, not Clojure-style vectors.",
    },
    {
        "prompt": "Write a Sema function that checks if a string is empty",
        "bad": "(defn empty? [s] (= s \"\"))",
        "good": "(define (empty? s) (string=? s \"\"))",
        "explanation": "Sema uses define, not Clojure's defn with vector args.",
    },
    {
        "prompt": "Write a Sema anonymous function that adds 1",
        "bad": "#(inc %)",
        "good": "#(+ % 1)",
        "explanation": "Sema doesn't have inc. Use + with 1.",
    },
    {
        "prompt": "Write a Sema keyword",
        "bad": ":ns/keyword",
        "good": ":keyword-name",
        "explanation": "Sema keywords are simple: :name, not namespaced like Clojure.",
    },
    {
        "prompt": "Write a Sema function that gets a map value",
        "bad": "(get-in m [:a :b])",
        "good": "(get-in m '(:a :b))",
        "explanation": "Sema uses lists for keys path, not vectors.",
    },
    {
        "prompt": "Write a Sema if expression",
        "bad": "(if true 1 2)",
        "good": "(if #t 1 2)",
        "explanation": "Both work, but #t/#f is the canonical Sema boolean syntax.",
    },
    {
        "prompt": "Write a Sema case expression",
        "bad": "(case x 1 \"one\" 2 \"two\")",
        "good": "(match x (1 \"one\") (2 \"two\") (_ \"other\"))",
        "explanation": "Sema uses match, not Clojure's case.",
    },
    {
        "prompt": "Write a Sema reduce with initial value",
        "bad": "(reduce + 0 [1 2 3])",
        "good": "(foldl + 0 '(1 2 3))",
        "explanation": "Sema uses foldl with a list, not Clojure's reduce with a vector.",
    },
    {
        "prompt": "Write a Sema string contains check",
        "bad": "(clojure.string/includes? \"hello\" \"ell\")",
        "good": "(string/contains? \"hello\" \"ell\")",
        "explanation": "Sema uses string/contains?, not clojure.string namespaces.",
    },
    {
        "prompt": "Write a Semap map update",
        "bad": "(update m :a inc)",
        "good": "(map/update m :a #(+ % 1))",
        "explanation": "Sema uses map/update with a function, not Clojure's update.",
    },
]


def parse_negative_examples(sema_path):
    """Generate correction examples for common Clojure/Scheme anti-patterns."""
    examples = []

    for ne in NEGATIVE_EXAMPLES:
        # Verify the good version works
        if sema_path and not vm_filter(sema_path, ne["good"]):
            continue

        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT_CODEGEN},
                {"role": "user", "content": ne["prompt"]},
                {"role": "assistant", "content": ne["good"]},
            ],
            "source": "negative_correction",
            "bad_code": ne["bad"],
            "explanation": ne["explanation"],
        })

    return examples


# ─── Main ─────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description="Generate SFT dataset for Sema code generation")
    parser.add_argument("--sema-path", default=None, help="Path to sema binary for VM filtering")
    parser.add_argument("--output", default="data/sft", help="Output directory")
    parser.add_argument("--use-llm", action="store_true", help="Use a frontier model for NL descriptions")
    parser.add_argument("--base-url", default=None)
    parser.add_argument("--api-key", default=None)
    parser.add_argument("--llm-model", default="accounts/fireworks/models/glm-5p2")
    parser.add_argument("--val-split", type=float, default=0.1, help="Validation set fraction")
    args = parser.parse_args()

    sema_path = args.sema_path
    if not sema_path:
        for c in [REPO_ROOT / "target" / "debug" / "sema",
                  REPO_ROOT / "target" / "release" / "sema"]:
            if c.exists():
                sema_path = str(c)
                break

    if sema_path:
        print(f"Using sema: {sema_path}")
    else:
        print("WARNING: No sema binary found — skipping VM filtering")

    llm_config = None
    if args.use_llm:
        base_url = args.base_url or os.environ.get("OPENAI_BASE_URL", "https://api.fireworks.ai/inference/v1")
        api_key = args.api_key or os.environ.get("FIREWORKS_API_KEY") or os.environ.get("OPENAI_API_KEY")
        if api_key:
            llm_config = {"base_url": base_url, "api_key": api_key, "model": args.llm_model}
            print(f"Using LLM for descriptions: {args.llm_model}")
        else:
            print("WARNING: --use-llm but no API key found")

    all_examples = []

    print("\n--- Source 1: Example .sema files ---")
    examples = parse_example_files(sema_path, use_llm=args.use_llm, llm_config=llm_config)
    print(f"  Extracted {len(examples)} examples")
    all_examples.extend(examples)

    print("\n--- Source 2: Doc entries ---")
    examples = parse_doc_entries(sema_path)
    print(f"  Extracted {len(examples)} examples")
    all_examples.extend(examples)

    print("\n--- Source 3: Eval test cases ---")
    examples = parse_eval_tests(sema_path, use_llm=args.use_llm, llm_config=llm_config)
    print(f"  Extracted {len(examples)} examples")
    all_examples.extend(examples)

    print("\n--- Source 4: Negative/correction examples ---")
    examples = parse_negative_examples(sema_path)
    print(f"  Extracted {len(examples)} examples")
    all_examples.extend(examples)

    # Deduplicate by assistant content
    seen = set()
    deduped = []
    for ex in all_examples:
        content = ex["messages"][-1]["content"]
        if content not in seen:
            seen.add(content)
            deduped.append(ex)

    print(f"\n--- Total: {len(all_examples)} -> {len(deduped)} after dedup ---")

    # Shuffle for train/val split
    import random
    random.seed(42)
    random.shuffle(deduped)

    val_count = int(len(deduped) * args.val_split)
    val_set = deduped[:val_count]
    train_set = deduped[val_count:]

    out_dir = SCRIPT_DIR / args.output
    out_dir.mkdir(parents=True, exist_ok=True)

    train_path = out_dir / "train.jsonl"
    val_path = out_dir / "val.jsonl"

    with train_path.open("w") as f:
        for ex in train_set:
            f.write(json.dumps({"messages": ex["messages"]}) + "\n")

    with val_path.open("w") as f:
        for ex in val_set:
            f.write(json.dumps({"messages": ex["messages"]}) + "\n")

    # Stats by source
    by_source = {}
    for ex in train_set + val_set:
        src = ex.get("source", "unknown").split(":")[0]
        by_source[src] = by_source.get(src, 0) + 1

    print(f"\n--- Dataset written ---")
    print(f"  Train: {len(train_set)} -> {train_path}")
    print(f"  Val:   {len(val_set)} -> {val_path}")
    print(f"\n  By source:")
    for src, count in sorted(by_source.items()):
        print(f"    {src}: {count}")


if __name__ == "__main__":
    main()
