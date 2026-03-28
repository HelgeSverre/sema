import { test, expect } from '@playwright/test';
import { execSync } from 'child_process';
import * as path from 'path';
import * as api from './helpers/api';

const BASE = 'http://localhost:3111';
const DB_PATH = path.resolve(__dirname, '..', 'e2e-test.db');

// ── DB helpers ──

function makeAdmin(username: string) {
  execSync(
    `sqlite3 "${DB_PATH}" "UPDATE users SET is_admin = 1 WHERE username = '${username}'"`,
  );
}

function getUserId(username: string): number {
  const result = execSync(
    `sqlite3 "${DB_PATH}" "SELECT id FROM users WHERE username = '${username}'"`,
    { encoding: 'utf-8' },
  );
  return parseInt(result.trim(), 10);
}

// ── Unique name helpers ──

let counter = 0;
function unique(prefix: string): string {
  counter++;
  return `${prefix}${counter}${Date.now()}`;
}

test.describe('Admin Panel', () => {
  // ────────────────────────────────────────────
  // 1. Admin page loads for admin user
  // ────────────────────────────────────────────
  test('admin page loads for admin user', async ({ page, request }) => {
    const username = unique('adm-load');
    const session = await api.register(request, username, `${username}@test.com`);
    makeAdmin(username);
    await api.setSession(page.context(), session);

    await page.goto('/admin');
    await expect(page).toHaveURL(/\/admin/);
    // Dashboard should be visible on load
    await expect(page.locator('.page-title')).toHaveText('Dashboard');
    await expect(page.locator('.stats-grid')).toBeVisible();
  });

  // ────────────────────────────────────────────
  // 2. Admin page returns 403 for non-admin
  // ────────────────────────────────────────────
  test('admin page returns 403 for non-admin', async ({ page, request }) => {
    const username = unique('adm-nonadm');
    const session = await api.register(request, username, `${username}@test.com`);
    await api.setSession(page.context(), session);

    const response = await page.goto('/admin');
    expect(response?.status()).toBe(403);
    const body = await page.textContent('body');
    expect(body).toContain('Admin access required');
  });

  // ────────────────────────────────────────────
  // 3. Admin page redirects to login when unauthenticated
  // ────────────────────────────────────────────
  test('admin page redirects to login when unauthenticated', async ({
    page,
  }) => {
    await page.goto('/admin');
    await page.waitForURL(/\/login/, { timeout: 10_000 });
    expect(page.url()).toContain('/login');
  });

  // ────────────────────────────────────────────
  // 4. Dashboard shows stats
  // ────────────────────────────────────────────
  test('dashboard shows stats', async ({ page, request }) => {
    const adminName = unique('adm-stats');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    // Create some users and packages to show in stats
    const user1 = unique('statsuser1');
    await api.register(request, user1, `${user1}@test.com`);
    const user2 = unique('statsuser2');
    await api.register(request, user2, `${user2}@test.com`);

    const token = await api.createToken(request, adminSession, 'stats-tok');
    await api.publishPackage(request, token, unique('statspkg'), '0.1.0');

    await api.setSession(page.context(), adminSession);

    // Navigate and wait for stats fetch
    const statsPromise = page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/stats') && r.ok(),
    );
    await page.goto('/admin');
    await statsPromise;

    // Stat values should be visible and numeric
    const statValues = page.locator('.stat-value');
    await expect(statValues.first()).toBeVisible();
    // Total users should be at least 3 (admin + 2 users)
    const totalUsersText = await statValues.nth(0).textContent();
    expect(parseInt(totalUsersText ?? '0', 10)).toBeGreaterThanOrEqual(3);
    // Packages should be at least 1
    const totalPkgsText = await statValues.nth(1).textContent();
    expect(parseInt(totalPkgsText ?? '0', 10)).toBeGreaterThanOrEqual(1);
  });

  // ────────────────────────────────────────────
  // 5. User search works
  // ────────────────────────────────────────────
  test('user search works', async ({ page, request }) => {
    const adminName = unique('adm-search');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    const alice = unique('searchable-alice');
    const bob = unique('searchable-bob');
    await api.register(request, alice, `${alice}@test.com`);
    await api.register(request, bob, `${bob}@test.com`);

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');

    // Switch to Users tab
    await page.locator('.sidebar-link', { hasText: 'Users' }).click();
    const usersLoaded = page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/users') && r.ok(),
    );
    await usersLoaded;

    // Type in search box — use a distinctive substring from alice's name
    const searchInput = page.locator(
      'div[x-show*="users"] .toolbar-search',
    );
    await expect(searchInput).toBeVisible();

    const searchResponse = page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/users') && r.url().includes('q=') && r.ok(),
    );
    await searchInput.fill(alice);
    await searchResponse;

    // Alice should be visible, bob should not
    const rows = page.locator('.admin-table tbody tr');
    await expect(rows.first()).toBeVisible();
    const tableText = await page.locator('.admin-table tbody').textContent();
    expect(tableText).toContain(alice);
    expect(tableText).not.toContain(bob);
  });

  // ────────────────────────────────────────────
  // 6. Ban user flow
  // ────────────────────────────────────────────
  test('ban user flow', async ({ page, request }) => {
    const adminName = unique('adm-ban');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    const bannable = unique('bannable');
    await api.register(request, bannable, `${bannable}@test.com`);

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');

    // Navigate to Users tab
    await page.locator('.sidebar-link', { hasText: 'Users' }).click();
    await page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/users') && r.ok(),
    );

    // Search for the bannable user to narrow down the list
    const searchInput = page.locator(
      'div[x-show*="users"] .toolbar-search',
    );
    const searchResponse = page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/users') && r.url().includes('q=') && r.ok(),
    );
    await searchInput.fill(bannable);
    await searchResponse;

    // Find the row containing the bannable user and click Ban
    const userRow = page.locator('.admin-table tbody tr', {
      hasText: bannable,
    });
    await expect(userRow).toBeVisible();

    // Click Ban — this opens the confirm dialog
    await userRow.locator('button', { hasText: 'Ban' }).click();

    // Confirm dialog should appear
    await expect(page.locator('.confirm-dialog')).toBeVisible();
    await expect(page.locator('.confirm-title')).toHaveText('Ban User');

    // Click the confirm button
    const banResponse = page.waitForResponse(
      (r) => r.url().includes('/ban') && r.ok(),
    );
    await page.locator('.confirm-dialog .action-btn-danger').click();
    await banResponse;

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 5_000 });
    const toastText = await page.locator('.toast-success').textContent();
    expect(toastText).toContain('Banned');

    // After reload the user should show as Banned
    await page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/users') && r.ok(),
    );
    const statusBadge = page
      .locator('.admin-table tbody tr', { hasText: bannable })
      .locator('.status-banned');
    await expect(statusBadge).toBeVisible({ timeout: 5_000 });
  });

  // ────────────────────────────────────────────
  // 7. Package management — yank all
  // ────────────────────────────────────────────
  test('package management — yank all', async ({ page, request }) => {
    const adminName = unique('adm-yank');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    const token = await api.createToken(request, adminSession, 'yank-tok');
    const pkgName = unique('yankpkg');
    await api.publishPackage(request, token, pkgName, '0.1.0');
    await api.publishPackage(request, token, pkgName, '0.2.0');

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');

    // Navigate to Packages tab
    await page.locator('.sidebar-link', { hasText: 'Packages' }).click();
    await page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/packages') && r.ok(),
    );

    // Find the package row
    const pkgRow = page.locator('.admin-table tbody tr', {
      hasText: pkgName,
    });
    await expect(pkgRow).toBeVisible();

    // Click Yank All — opens confirm dialog
    await pkgRow.locator('button', { hasText: 'Yank All' }).click();

    await expect(page.locator('.confirm-dialog')).toBeVisible();
    await expect(page.locator('.confirm-title')).toHaveText(
      'Yank All Versions',
    );

    // Confirm
    const yankResponse = page.waitForResponse(
      (r) => r.url().includes('/yank-all') && r.ok(),
    );
    await page.locator('.confirm-dialog .action-btn-warn').click();
    await yankResponse;

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 5_000 });
    const toastText = await page.locator('.toast-success').textContent();
    expect(toastText).toContain('Yanked');
  });

  // ────────────────────────────────────────────
  // 8. Package management — remove
  // ────────────────────────────────────────────
  test('package management — remove', async ({ page, request }) => {
    const adminName = unique('adm-remove');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    const token = await api.createToken(request, adminSession, 'rm-tok');
    const pkgName = unique('removepkg');
    await api.publishPackage(request, token, pkgName, '1.0.0');

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');

    // Navigate to Packages tab
    await page.locator('.sidebar-link', { hasText: 'Packages' }).click();
    await page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/packages') && r.ok(),
    );

    // Find the package row
    const pkgRow = page.locator('.admin-table tbody tr', {
      hasText: pkgName,
    });
    await expect(pkgRow).toBeVisible();

    // Click Remove — opens confirm dialog
    await pkgRow.locator('button', { hasText: 'Remove' }).click();

    await expect(page.locator('.confirm-dialog')).toBeVisible();
    await expect(page.locator('.confirm-title')).toHaveText('Remove Package');

    // Confirm
    const removeResponse = page.waitForResponse(
      (r) =>
        r.url().includes('/api/v1/admin/packages/') &&
        r.request().method() === 'DELETE' &&
        r.ok(),
    );
    await page.locator('.confirm-dialog .action-btn-danger').click();
    await removeResponse;

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 5_000 });
    const toastText = await page.locator('.toast-success').textContent();
    expect(toastText).toContain('Removed');

    // Package should no longer appear in the list after reload
    await page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/packages') && r.ok(),
    );
    await expect(
      page.locator('.admin-table tbody tr', { hasText: pkgName }),
    ).toHaveCount(0, { timeout: 5_000 });
  });

  // ────────────────────────────────────────────
  // 9. Audit log displays entries
  // ────────────────────────────────────────────
  test('audit log displays entries', async ({ page, request }) => {
    const adminName = unique('adm-audit');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    // Publish a package so there is audit activity
    const token = await api.createToken(request, adminSession, 'audit-tok');
    const pkgName = unique('auditpkg');
    await api.publishPackage(request, token, pkgName, '1.0.0');

    // Ban a user to generate an audit entry
    const victim = unique('auditvictim');
    await api.register(request, victim, `${victim}@test.com`);
    const victimId = getUserId(victim);
    await request.post(`${BASE}/api/v1/admin/users/${victimId}/ban`, {
      headers: { cookie: `session=${adminSession}` },
    });

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');

    // Switch to Audit Log tab
    await page.locator('.sidebar-link', { hasText: 'Audit Log' }).click();
    const auditLoaded = page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/audit') && r.ok(),
    );
    await auditLoaded;

    // Audit entries should be visible
    const entries = page.locator('.audit-entry');
    await expect(entries.first()).toBeVisible({ timeout: 5_000 });
    const count = await entries.count();
    expect(count).toBeGreaterThanOrEqual(1);

    // At least one entry should mention the admin actor
    const auditText = await page
      .locator('div[x-show*="audit"]')
      .textContent();
    expect(auditText).toContain(adminName);
  });

  // ────────────────────────────────────────────
  // 10. Reports — submit and view
  // ────────────────────────────────────────────
  test('reports — submit and view', async ({ page, request }) => {
    const adminName = unique('adm-report');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    // Regular user submits a report
    const reporter = unique('reporter');
    const reporterSession = await api.register(
      request,
      reporter,
      `${reporter}@test.com`,
    );

    const targetPkg = unique('reportedpkg');
    const reportRes = await request.post(`${BASE}/api/v1/reports`, {
      data: {
        target_type: 'package',
        target_name: targetPkg,
        report_type: 'spam',
        reason: 'This package is spam',
      },
      headers: { cookie: `session=${reporterSession}` },
    });
    expect(reportRes.ok()).toBeTruthy();

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');

    // Switch to Reports tab
    await page.locator('.sidebar-link', { hasText: 'Reports' }).click();
    const reportsLoaded = page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/reports') && r.ok(),
    );
    await reportsLoaded;

    // The report should be visible
    const reportsSection = page.locator('div[x-show*="reports"]');
    await expect(
      reportsSection.locator(`text=${targetPkg}`),
    ).toBeVisible({ timeout: 5_000 });
    const sectionText = await reportsSection.textContent();
    expect(sectionText).toContain('This package is spam');
    expect(sectionText).toContain(reporter);
  });

  // ────────────────────────────────────────────
  // 11. Reports — dismiss
  // ────────────────────────────────────────────
  test('reports — dismiss', async ({ page, request }) => {
    const adminName = unique('adm-dismiss');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    // Submit a report
    const reporter = unique('dismissrep');
    const reporterSession = await api.register(
      request,
      reporter,
      `${reporter}@test.com`,
    );

    const targetPkg = unique('dismissedpkg');
    await request.post(`${BASE}/api/v1/reports`, {
      data: {
        target_type: 'package',
        target_name: targetPkg,
        report_type: 'abuse',
        reason: 'Abusive content in readme',
      },
      headers: { cookie: `session=${reporterSession}` },
    });

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');

    // Switch to Reports tab
    await page.locator('.sidebar-link', { hasText: 'Reports' }).click();
    await page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/reports') && r.ok(),
    );

    // Find the report row and click Dismiss
    const reportRow = page.locator('div[x-show*="reports"]').locator('div', {
      hasText: targetPkg,
    });
    await expect(reportRow.first()).toBeVisible({ timeout: 5_000 });

    const dismissResponse = page.waitForResponse(
      (r) => r.url().includes('/dismiss') && r.ok(),
    );
    // The Dismiss button is next to the report
    await page
      .locator('div[x-show*="reports"]')
      .locator('button', { hasText: 'Dismiss' })
      .first()
      .click();
    await dismissResponse;

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 5_000 });
    const toastText = await page.locator('.toast-success').textContent();
    expect(toastText).toContain('Dismissed');

    // After reload, report should be gone from open list
    await page.waitForResponse(
      (r) => r.url().includes('/api/v1/admin/reports') && r.ok(),
    );
    // Wait a beat for Alpine to re-render
    await page.waitForTimeout(500);
    const remainingText = await page
      .locator('div[x-show*="reports"]')
      .textContent();
    expect(remainingText).not.toContain(targetPkg);
  });

  // ────────────────────────────────────────────
  // 12. Admin nav link visible only for admins
  // ────────────────────────────────────────────
  test('admin nav link visible only for admins', async ({ page, request }) => {
    // Non-admin user should not see the Admin link
    const normalUser = unique('navnormal');
    const normalSession = await api.register(
      request,
      normalUser,
      `${normalUser}@test.com`,
    );
    await api.setSession(page.context(), normalSession);
    await page.goto('/');
    await expect(page.getByTestId('nav-admin')).toHaveCount(0);

    // Admin user should see the Admin link
    const adminUser = unique('navadmin');
    const adminSession = await api.register(
      request,
      adminUser,
      `${adminUser}@test.com`,
    );
    makeAdmin(adminUser);

    // Need a new context to set the admin session
    const adminContext = await page.context().browser()!.newContext();
    await api.setSession(adminContext, adminSession);
    const adminPage = await adminContext.newPage();
    await adminPage.goto('/');
    await expect(adminPage.getByTestId('nav-admin')).toBeVisible();
    await adminContext.close();
  });
});
