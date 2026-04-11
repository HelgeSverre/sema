import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '.',
  testMatch: '*.spec.ts',
  timeout: 30000,
  retries: 0,
  workers: 1,
  use: {
    baseURL: 'http://127.0.0.1:18888',
  },
  webServer: {
    command: 'cargo run -- notebook serve -p 18888 examples/notebook/demo.sema-nb',
    port: 18888,
    cwd: '../../../../',
    reuseExistingServer: true,
    timeout: 60000,
    stdout: 'pipe',
    stderr: 'pipe',
  },
});
