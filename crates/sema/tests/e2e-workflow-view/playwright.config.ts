import { defineConfig } from '@playwright/test';

// E2E for `sema workflow view` — the dynamic-workflow dashboard. Boots the real
// binary as the viewer server (pointed at a committed fixture run-dir) and asserts
// the AlpineJS tree renders the journal. Mirrors the notebook e2e harness.
export default defineConfig({
  testDir: '.',
  testMatch: '*.spec.ts',
  timeout: 30000,
  retries: 0,
  workers: 1,
  use: {
    baseURL: 'http://127.0.0.1:18899',
  },
  webServer: {
    command:
      'cargo run --bin sema -- workflow view --run-dir crates/sema/tests/fixtures/workflow/viewer-runs --port 18899',
    port: 18899,
    cwd: '../../../../',
    reuseExistingServer: true,
    timeout: 120000,
    stdout: 'pipe',
    stderr: 'pipe',
  },
});
