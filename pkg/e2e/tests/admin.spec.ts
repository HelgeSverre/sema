import { test } from '@playwright/test';

// Future Admin Capabilities
// These tests are stubs for admin features not yet implemented.

test.describe('Admin (future)', () => {
  test.skip('ban user — prevents login and publishing', async () => {
    // TODO: POST /api/v1/admin/users/{id}/ban
  });

  test.skip('remove package — hides from search and browse', async () => {
    // TODO: DELETE /api/v1/admin/packages/{name}
  });

  test.skip('force yank all versions', async () => {
    // TODO: POST /api/v1/admin/packages/{name}/yank-all
  });

  test.skip('transfer package ownership', async () => {
    // TODO: POST /api/v1/admin/packages/{name}/transfer
  });

  test.skip('view audit log', async () => {
    // TODO: GET /api/v1/admin/audit-log
  });
});
