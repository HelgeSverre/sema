.PHONY: dev seed seed-stress test test-e2e test-sqlite test-postgres test-mysql test-all-drivers clean

dev:
	cargo run

seed:
	bash seed.sh

seed-stress: seed
	python3 seed_stress.py

test:
	cargo test

test-e2e:
	cd e2e && npx playwright test

test-e2e-headed:
	cd e2e && npx playwright test --headed

test-sqlite:
	docker compose -f docker-compose.test.yml run --rm test-sqlite

test-postgres:
	docker compose -f docker-compose.test.yml up -d postgres
	docker compose -f docker-compose.test.yml run --rm test-postgres
	docker compose -f docker-compose.test.yml stop postgres

test-mysql:
	docker compose -f docker-compose.test.yml up -d mysql
	docker compose -f docker-compose.test.yml run --rm test-mysql
	docker compose -f docker-compose.test.yml stop mysql

test-all-drivers:
	docker compose -f docker-compose.test.yml up -d postgres mysql
	docker compose -f docker-compose.test.yml run --rm test-sqlite
	docker compose -f docker-compose.test.yml run --rm test-postgres
	docker compose -f docker-compose.test.yml run --rm test-mysql
	docker compose -f docker-compose.test.yml down -v

clean:
	rm -f data/registry.db data/registry.db-wal data/registry.db-shm
	rm -rf data/blobs
	rm -f e2e/e2e-test.db*
	rm -rf e2e/e2e-blobs
