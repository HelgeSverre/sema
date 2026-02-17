.PHONY: all build release install uninstall test test-embedding-bench test-http test-llm check clippy fmt fmt-check clean run lint examples examples-vm test-providers fuzz fuzz-reader fuzz-eval setup bench-1m bench-10m bench-100m site-dev site-build site-preview site-deploy coverage coverage-html bench bench-vm bench-tree bench-save profile profile-vm profile-tree

build:
	cargo build

release:
	cargo build --release

install:
	cargo install --path crates/sema

uninstall:
	cargo uninstall sema

test:
	cargo test

test-embedding-bench:
	cargo test -p sema --test embedding_bench -- --ignored --nocapture

test-http:
	cargo test -p sema --test http_test -- --ignored --nocapture

test-llm:
	cargo test -p sema --test llm_test -- --ignored --nocapture

check:
	cargo check

clippy:
	cargo clippy -p sema-core -p sema-reader -p sema-eval -p sema-llm -p sema-stdlib -p sema-vm -p sema -- -D warnings

fmt:
	cargo fmt

fmt-check:
	cargo fmt -- --check

clean:
	cargo clean

run:
	cargo run

lint: fmt-check clippy

examples: build
	@echo "=== Running examples ==="
	@for f in examples/*.sema; do \
		echo "--- $$f ---"; \
		cargo run --quiet -- --no-llm "$$f" || true; \
	done
	@echo "=== Running stdlib examples ==="
	@for f in examples/stdlib/*.sema; do \
		echo "--- $$f ---"; \
		cargo run --quiet -- --no-llm "$$f" || true; \
	done

examples-vm: build
	@echo "=== Running examples (--vm) ==="
	@failed=""; \
	for f in examples/*.sema; do \
		echo "--- $$f ---"; \
		if ! cargo run --quiet -- --vm --no-llm "$$f"; then \
			failed="$$failed $$f"; \
		fi; \
	done; \
	echo "=== Running stdlib examples (--vm) ==="; \
	for f in examples/stdlib/*.sema; do \
		echo "--- $$f ---"; \
		if ! cargo run --quiet -- --vm --no-llm "$$f"; then \
			failed="$$failed $$f"; \
		fi; \
	done; \
	if [ -n "$$failed" ]; then \
		echo ""; \
		echo "=== FAILED (--vm) ==="; \
		for f in $$failed; do echo "  $$f"; done; \
		echo ""; \
	else \
		echo ""; \
		echo "=== ALL PASSED (--vm) ==="; \
	fi

test-providers: build
	@echo "=== Testing all LLM providers ==="
	cargo run --quiet -- examples/providers/test-all.sema

test-provider-%: build
	cargo run --quiet -- examples/providers/test-$*.sema

setup:
	rustup toolchain install nightly
	cargo install cargo-fuzz

fuzz: fuzz-reader fuzz-eval

fuzz-reader:
	cd crates/sema-reader && rustup run nightly cargo fuzz run fuzz_read -- -max_total_time=60
	cd crates/sema-reader && rustup run nightly cargo fuzz run fuzz_read_many -- -max_total_time=60

fuzz-eval:
	cd crates/sema-eval && rustup run nightly cargo fuzz run fuzz_eval -- -max_total_time=120 -timeout=10

bench-1m: release
	time ./target/release/sema examples/benchmarks/1brc.sema -- bench-1m.txt

bench-10m: release
	time ./target/release/sema examples/benchmarks/1brc.sema -- bench-10m.txt

bench-100m: release
	time ./target/release/sema examples/benchmarks/1brc.sema -- bench-100m.txt

all: lint test build

# Website
.PHONY: site-dev site-build site-preview site-deploy

site-dev:
	cd website && npm run dev

site-build:
	cd website && npm run build

site-preview: site-build
	cd website && npm run preview

site-deploy: site-build
	cd website && npx vercel --prod

# Playground
.PHONY: playground-build playground-dev playground-deploy

playground-build:
	cd crates/sema-wasm && wasm-pack build --target web --out-dir ../../playground/pkg
	cd playground && node build.mjs

playground-dev: playground-build
	cd playground && npx serve -l 8787

playground-deploy: playground-build
	cd playground && npx vercel --prod

# Coverage
coverage:
	cargo llvm-cov --workspace --lcov --output-path lcov.info

coverage-html:
	cargo llvm-cov --workspace --html
	@echo "Coverage report: target/llvm-cov/html/index.html"

# Benchmarking
BENCH_RUNS ?= 10
BENCH_WARMUP ?= 3

bench: release
	@./scripts/bench.sh --mode both --runs $(BENCH_RUNS) --warmup $(BENCH_WARMUP)

bench-vm: release
	@./scripts/bench.sh --mode vm --runs $(BENCH_RUNS) --warmup $(BENCH_WARMUP)

bench-tree: release
	@./scripts/bench.sh --mode tree --runs $(BENCH_RUNS) --warmup $(BENCH_WARMUP)

bench-save: release
	@mkdir -p target/bench
	@./scripts/bench.sh --mode both --runs $(BENCH_RUNS) --warmup $(BENCH_WARMUP) \
		--export target/bench/bench-$$(git rev-parse --short HEAD 2>/dev/null || echo "nogit").json

# Profiling (requires: cargo install samply)
PROFILE_DIR := target/profiles
PROFILE_BENCH ?= tak
PROFILE_MODE ?= vm

profile:
	@mkdir -p $(PROFILE_DIR)
	RUSTFLAGS="-C force-frame-pointers=yes" cargo build --profile release-with-debug -p sema
	@modeflag=""; if [ "$(PROFILE_MODE)" = "vm" ]; then modeflag="--vm"; fi; \
	samply record --save-only --output $(PROFILE_DIR)/$(PROFILE_BENCH)-$(PROFILE_MODE).json -- \
		./target/release-with-debug/sema --no-llm $$modeflag examples/benchmarks/$(PROFILE_BENCH).sema
	@echo "Profile saved: $(PROFILE_DIR)/$(PROFILE_BENCH)-$(PROFILE_MODE).json"
	@echo "Open with: samply load $(PROFILE_DIR)/$(PROFILE_BENCH)-$(PROFILE_MODE).json"

profile-vm: 
	@$(MAKE) profile PROFILE_MODE=vm PROFILE_BENCH=$(PROFILE_BENCH)

profile-tree:
	@$(MAKE) profile PROFILE_MODE=tree PROFILE_BENCH=$(PROFILE_BENCH)
