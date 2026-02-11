.PHONY: all build release install uninstall test check clippy fmt fmt-check clean run lint examples test-providers

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

check:
	cargo check

clippy:
	cargo clippy -- -D warnings

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
	@echo "=== Running all examples ==="
	@for f in examples/*.sema; do \
		echo "--- $$f ---"; \
		cargo run --quiet -- --no-llm "$$f" || true; \
	done

test-providers: build
	@echo "=== Testing all LLM providers ==="
	cargo run --quiet -- examples/providers/test-all.sema

test-provider-%: build
	cargo run --quiet -- examples/providers/test-$*.sema

all: lint test build
