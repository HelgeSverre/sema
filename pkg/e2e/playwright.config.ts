import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: './tests',
  timeout: 60_000,
  retries: 0,
  workers: 1, // sequential — tests share DB state
  use: {
    baseURL: 'http://localhost:3111',
    trace: 'on-first-retry',
  },
  webServer: {
    command:
      'DATABASE_URL=sqlite://e2e/e2e-test.db?mode=rwc BLOB_DIR=e2e/e2e-blobs BASE_URL=http://localhost:3111 PORT=3111 cargo run',
    port: 3111,
    reuseExistingServer: true,
    timeout: 120_000, // cargo build can take a while
    cwd: `${__dirname}/..`, // run from pkg/ so static files and templates resolve correctly
  },
});
