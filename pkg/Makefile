.PHONY: dev seed seed-stress test test-e2e clean

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

clean:
	rm -f data/registry.db data/registry.db-wal data/registry.db-shm
	rm -rf data/blobs
	rm -f e2e/e2e-test.db*
	rm -rf e2e/e2e-blobs
