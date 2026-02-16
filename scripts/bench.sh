#!/usr/bin/env bash
set -euo pipefail

SEMA_BIN="${SEMA_BIN:-./target/release/sema}"

BENCHMARKS=(
  "examples/benchmarks/tak.sema"
  "examples/benchmarks/nqueens.sema"
  "examples/benchmarks/deriv.sema"
  "examples/benchmarks/bench-features.sema"
)

MODE="both"
RUNS=10
WARMUP=3
EXPORT=""

usage() {
  echo "Usage: $0 [--mode tree|vm|both] [--runs N] [--warmup N] [--export FILE]"
  exit 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode)   MODE="$2"; shift 2 ;;
    --runs)   RUNS="$2"; shift 2 ;;
    --warmup) WARMUP="$2"; shift 2 ;;
    --export) EXPORT="$2"; shift 2 ;;
    -h|--help) usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

if [[ "$MODE" != "tree" && "$MODE" != "vm" && "$MODE" != "both" ]]; then
  echo "Error: --mode must be tree, vm, or both"
  exit 1
fi

if ! command -v hyperfine &>/dev/null; then
  echo "Error: hyperfine is not installed. Install with: brew install hyperfine"
  exit 1
fi

if [[ ! -x "$SEMA_BIN" ]]; then
  echo "Error: $SEMA_BIN not found. Run 'cargo build --release' first."
  exit 1
fi

GIT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
echo "═══════════════════════════════════════════════════"
echo "  Sema Benchmarks"
echo "  git: $GIT_SHA  date: $(date '+%Y-%m-%d %H:%M:%S')"
echo "  mode: $MODE  runs: $RUNS  warmup: $WARMUP"
echo "  bin: $SEMA_BIN"
echo "═══════════════════════════════════════════════════"
echo

for bench in "${BENCHMARKS[@]}"; do
  name=$(basename "$bench" .sema)

  if [[ ! -f "$bench" ]]; then
    echo "Warning: $bench not found, skipping"
    continue
  fi

  echo ">>> $name"

  HYPERFINE_ARGS=(--runs "$RUNS" --warmup "$WARMUP" --style full --ignore-failure)

  if [[ -n "$EXPORT" ]]; then
    HYPERFINE_ARGS+=(--export-json "${EXPORT%.json}-${name}.json")
  fi

  case "$MODE" in
    tree)
      hyperfine "${HYPERFINE_ARGS[@]}" \
        -n "tree: $name" "$SEMA_BIN --no-llm $bench"
      ;;
    vm)
      hyperfine "${HYPERFINE_ARGS[@]}" \
        -n "vm: $name" "$SEMA_BIN --no-llm --vm $bench"
      ;;
    both)
      hyperfine "${HYPERFINE_ARGS[@]}" \
        -n "tree: $name" "$SEMA_BIN --no-llm $bench" \
        -n "vm: $name" "$SEMA_BIN --no-llm --vm $bench"
      ;;
  esac

  echo
done
