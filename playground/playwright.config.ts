import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: './tests',
  timeout: 60000,
  use: {
    baseURL: 'http://localhost:8787',
  },
  webServer: {
    command: 'npx serve -l 8787',
    port: 8787,
    reuseExistingServer: true,
  },
});
