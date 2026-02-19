#!/bin/bash
set -euo pipefail

DATA_FILE="${1:-/data/measurements.txt}"
RESULTS_FILE="/results/results-simple.json"

if [ ! -f "$DATA_FILE" ]; then
    echo "ERROR: Data file not found: $DATA_FILE"
    echo "Mount it with: docker run -v /path/to/data.txt:/data/measurements.txt:ro ..."
    exit 1
fi

ROW_COUNT=$(wc -l < "$DATA_FILE")
echo "=== 1BRC Lisp Dialect Benchmark (Simple/Idiomatic) ==="
echo "Data file: $DATA_FILE ($ROW_COUNT rows)"
echo ""

mkdir -p /results
rm -f "$RESULTS_FILE.tmp"

# Run a benchmark and capture wall-clock time
run_bench() {
    local name="$1"
    shift
    local cmd=("$@")

    echo "--- $name ---"

    # Check if the binary exists
    if ! command -v "${cmd[0]}" &>/dev/null; then
        echo "  SKIPPED (${cmd[0]} not found)"
        echo "{\"name\": \"$name\", \"best_ms\": null, \"error\": \"not installed\"}" >> "$RESULTS_FILE.tmp"
        echo ""
        return
    fi

    # Warm the file system cache
    cat "$DATA_FILE" > /dev/null 2>&1

    # Run 3 times, take the best
    local best=""
    for run in 1 2 3; do
        local t0=$(date +%s%N)
        timeout 600 "${cmd[@]}" > "/results/${name}-simple.stdout" 2> "/results/${name}-simple.stderr" || {
            local ec=$?
            echo "  Run $run: FAILED (exit $ec)"
            head -5 "/results/${name}-simple.stderr" 2>/dev/null | sed 's/^/    /'
            continue
        }
        local t1=$(date +%s%N)
        local elapsed_ms=$(( (t1 - t0) / 1000000 ))
        echo "  Run $run: ${elapsed_ms} ms"
        if [ -z "$best" ] || [ "$elapsed_ms" -lt "$best" ]; then
            best="$elapsed_ms"
        fi
    done

    if [ -n "$best" ]; then
        echo "  Best: ${best} ms"
        echo "{\"name\": \"$name\", \"best_ms\": $best, \"rows\": $ROW_COUNT}" >> "$RESULTS_FILE.tmp"
    else
        echo "  ALL RUNS FAILED"
        echo "{\"name\": \"$name\", \"best_ms\": null, \"error\": \"failed\"}" >> "$RESULTS_FILE.tmp"
    fi
    echo ""
}

echo "=== Running simple/idiomatic benchmarks (3 runs each, best of 3) ==="
echo ""

# ── Native compilers ──
run_bench "sbcl"     sbcl --script /bench/simple/1brc.lisp "$DATA_FILE"
run_bench "chez"     scheme --script /bench/simple/1brc.ss "$DATA_FILE"
run_bench "chicken"  /bench/1brc-chicken-simple "$DATA_FILE"
run_bench "gambit"   env BENCH_FILE="$DATA_FILE" /bench/1brc-gambit-simple

# ── JIT-compiled ──
run_bench "fennel"   fennel --lua luajit /bench/simple/1brc.fnl "$DATA_FILE"

# ── JVM-based ──
run_bench "clojure"  clojure -M /bench/simple/1brc.clj "$DATA_FILE"
run_bench "kawa"     kawa --script /bench/simple/1brc.kawa.scm "$DATA_FILE"

# ── Tree-walking interpreters ──
run_bench "sema"     sema --no-llm /bench/simple/1brc.sema -- "$DATA_FILE"
run_bench "sema-vm"  sema --no-llm --vm /bench/simple/1brc.sema -- "$DATA_FILE"

# ── Bytecode VMs / Interpreters ──
run_bench "guile"    guile /bench/simple/1brc.scm "$DATA_FILE"
run_bench "gauche"   gosh /bench/simple/1brc.gauche.scm "$DATA_FILE"
run_bench "janet"    janet /bench/simple/1brc.janet "$DATA_FILE"
run_bench "ecl"      ecl --load /bench/1brc-simple-ecl.fas -- "$DATA_FILE"
run_bench "emacs"    emacs --batch -l /bench/1brc-simple.elc "$DATA_FILE"
run_bench "newlisp"  newlisp /bench/simple/1brc.lsp "$DATA_FILE"
run_bench "picolisp" picolisp /bench/simple/1brc.l "$DATA_FILE"

# ── Build JSON results ──
echo "[" > "$RESULTS_FILE"
if [ -f "$RESULTS_FILE.tmp" ]; then
    sed '$!s/$/,/' "$RESULTS_FILE.tmp" >> "$RESULTS_FILE"
fi
echo "]" >> "$RESULTS_FILE"
rm -f "$RESULTS_FILE.tmp"

# ── Summary table ──
echo "=== Results (Simple/Idiomatic) ==="
echo ""
printf "%-15s %10s %8s\n" "Dialect" "Best (ms)" "vs Best"
printf "%-15s %10s %8s\n" "───────" "─────────" "───────"

python3 -c "
import json, sys
with open('$RESULTS_FILE') as f:
    data = json.load(f)
results = [(r['name'], r['best_ms']) for r in data if r.get('best_ms')]
results.sort(key=lambda x: x[1])
if results:
    baseline = results[0][1]
    for name, ms in results:
        ratio = ms / baseline
        print(f'{name:<15s} {ms:>10d} {ratio:>7.1f}x')
for r in data:
    if not r.get('best_ms'):
        print(f'{r[\"name\"]:<15s} {\"FAILED\":>10s}')
" 2>/dev/null || cat "$RESULTS_FILE"

echo ""
echo "Results written to $RESULTS_FILE"
