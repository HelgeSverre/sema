.PHONY: all build release install uninstall test test-http check clippy fmt fmt-check clean run lint examples test-providers fuzz fuzz-reader fuzz-eval setup bench-1m bench-10m bench-100m site-dev site-build site-preview site-deploy

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

test-http:
	cargo test -p sema --test integration_test test_http -- --ignored

check:
	cargo check

clippy:
	cargo clippy -p sema-core -p sema-reader -p sema-eval -p sema-llm -p sema-stdlib -p sema -- -D warnings

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
	cd playground/crate && wasm-pack build --target web --out-dir ../pkg

playground-dev: playground-build
	cd playground && npx serve -l 8787

playground-deploy: playground-build
	cd playground && npx vercel --prod
