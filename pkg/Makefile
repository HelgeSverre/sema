.PHONY: dev dev-docker down run seed seed-stress reset-db \
	test test-e2e test-e2e-headed test-sqlite test-postgres test-mysql test-all-drivers \
	clean help

# Preferred port. `dev`/`dev-docker` start here and bump to the next free port if it's
# taken, then point both the server and the seed at the same one. Override: `make dev PORT=4000`.
PORT ?= 3000
# Public URL for the simple `seed`/`run` targets (assume the default port).
BASE_URL ?= http://localhost:$(PORT)

help: ## Show available targets
	@grep -hE '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'

## ── Local development ──────────────────────────────────────

dev: reset-db ## Start the registry locally (cargo) on a fresh DB and seed it
	@start=$(PORT); port=$$start; \
	while lsof -iTCP:$$port -sTCP:LISTEN -t >/dev/null 2>&1; do port=$$((port+1)); done; \
	base="http://localhost:$$port"; \
	[ "$$port" = "$$start" ] || echo "Port $$start is busy — using $$port instead."; \
	echo "Starting sema-pkg on $$base — seeding once it's healthy (Ctrl-C to stop)..."; \
	SEED_MODE=local BASE_URL="$$base" bash seed.sh --wait & \
	PORT="$$port" BASE_URL="$$base" cargo run

run: ## Start the registry locally without resetting or seeding
	cargo run

seed: ## Seed a registry that is already running (no reset)
	BASE_URL="$(BASE_URL)" bash seed.sh

seed-stress: seed ## Seed, then bulk-load synthetic data (local SQLite only)
	python3 seed_stress.py

reset-db: ## Delete the local registry DB + blobs so the next seed is fresh
	rm -f data/registry.db data/registry.db-wal data/registry.db-shm
	rm -rf data/blobs

## ── Docker development ─────────────────────────────────────

dev-docker: ## Build + start the registry in Docker on a fresh DB and seed it
	docker compose down 2>/dev/null || true
	$(MAKE) reset-db
	@start=$(PORT); port=$$start; \
	while lsof -iTCP:$$port -sTCP:LISTEN -t >/dev/null 2>&1; do port=$$((port+1)); done; \
	base="http://localhost:$$port"; \
	[ "$$port" = "$$start" ] || echo "Host port $$start is busy — using $$port instead."; \
	PORT="$$port" BASE_URL="$$base" docker compose up --build -d; \
	SEED_MODE=docker BASE_URL="$$base" bash seed.sh --wait; \
	echo ""; \
	echo "Registry running in Docker at $$base  (admin: helge / 123123123)"; \
	echo "Tailing logs — Ctrl-C stops the tail; the container keeps running. Use 'make down' to stop it."; \
	PORT="$$port" docker compose logs -f

down: ## Stop the Docker registry
	docker compose down

## ── Tests ──────────────────────────────────────────────────

test: ## Run the Rust test suite
	cargo test

test-e2e: ## Run Playwright end-to-end tests
	cd e2e && npx playwright test

test-e2e-headed: ## Run Playwright end-to-end tests with a visible browser
	cd e2e && npx playwright test --headed

test-sqlite: ## Run the test suite against SQLite (Docker)
	docker compose -f docker-compose.test.yml run --rm test-sqlite

test-postgres:
	docker compose -f docker-compose.test.yml up -d postgres
	docker compose -f docker-compose.test.yml run --rm test-postgres
	docker compose -f docker-compose.test.yml stop postgres

test-mysql:
	docker compose -f docker-compose.test.yml up -d mysql
	docker compose -f docker-compose.test.yml run --rm test-mysql
	docker compose -f docker-compose.test.yml stop mysql

test-all-drivers: ## Run the test suite against SQLite, Postgres, and MySQL (Docker)
	docker compose -f docker-compose.test.yml up -d postgres mysql
	docker compose -f docker-compose.test.yml run --rm test-sqlite
	docker compose -f docker-compose.test.yml run --rm test-postgres
	docker compose -f docker-compose.test.yml run --rm test-mysql
	docker compose -f docker-compose.test.yml down -v

clean: ## Remove local + e2e databases and blobs
	rm -f data/registry.db data/registry.db-wal data/registry.db-shm
	rm -rf data/blobs
	rm -f e2e/e2e-test.db*
	rm -rf e2e/e2e-blobs
