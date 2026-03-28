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
    // Wait for stats grid to be visible — proves dashboard tab loaded with data
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });
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

    // Navigate and wait for stats to render
    await page.goto('/admin');
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Stat values should be visible and numeric
    const statValues = page.locator('.stat-value');
    await expect(statValues.first()).toBeVisible({ timeout: 10000 });
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
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Switch to Users tab and wait for table rows to appear
    await page.locator('.sidebar-link', { hasText: 'Users' }).click();
    await page.waitForSelector('.admin-table tbody tr', { timeout: 10000 });

    // Type in search box
    const searchInput = page.locator('.toolbar-search').first();
    await expect(searchInput).toBeVisible({ timeout: 10000 });

    await searchInput.fill(alice);
    // Wait for debounce + API response + re-render
    await page.waitForTimeout(1000);
    await page.waitForSelector(`.admin-table tbody td:has-text("${alice}")`, { timeout: 10000 });

    // Alice should be visible, bob should not
    const visibleRows = page.locator('.admin-table tbody tr:visible');
    await expect(visibleRows.first()).toBeVisible();
    await expect(page.locator(`.admin-table td:has-text("${alice}")`).first()).toBeVisible();
    await expect(page.locator(`.admin-table td:has-text("${bob}")`)).toHaveCount(0);
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
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Navigate to Users tab and wait for table to load
    await page.locator('.sidebar-link', { hasText: 'Users' }).click();
    await page.waitForSelector('.admin-table tbody tr', { timeout: 10000 });

    // Search for the bannable user to narrow down the list
    const searchInput = page.locator(
      'div[x-show*="users"] .toolbar-search',
    );
    await searchInput.fill(bannable);
    await page.waitForSelector(`.admin-table tbody tr:has-text("${bannable}")`, { timeout: 10000 });

    // Find the row containing the bannable user and click Ban
    const userRow = page.locator('.admin-table tbody tr', {
      hasText: bannable,
    });
    await expect(userRow).toBeVisible();

    // Click Ban — this opens the confirm dialog
    await userRow.locator('button', { hasText: 'Ban' }).click();

    // Confirm dialog should appear
    await expect(page.locator('.confirm-dialog')).toBeVisible({ timeout: 10000 });
    await expect(page.locator('.confirm-title')).toHaveText('Ban User');

    // Click the confirm button and wait for toast
    await page.locator('.confirm-dialog .action-btn-danger').click();

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 10000 });
    const toastText = await page.locator('.toast-success').textContent();
    expect(toastText).toContain('Banned');

    // After the action the user should show as Banned in the table
    const statusBadge = page
      .locator('.admin-table tbody tr', { hasText: bannable })
      .locator('.status-banned');
    await expect(statusBadge).toBeVisible({ timeout: 10000 });
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
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Navigate to Packages tab and wait for table rows
    await page.locator('.sidebar-link', { hasText: 'Packages' }).click();
    await page.waitForSelector('.admin-table tbody tr', { timeout: 10000 });

    // Find the package row
    const pkgRow = page.locator('.admin-table tbody tr', {
      hasText: pkgName,
    });
    await expect(pkgRow).toBeVisible();

    // Click Yank All — opens confirm dialog
    await pkgRow.locator('button', { hasText: 'Yank All' }).click();

    await expect(page.locator('.confirm-dialog')).toBeVisible({ timeout: 10000 });
    await expect(page.locator('.confirm-title')).toHaveText(
      'Yank All Versions',
    );

    // Confirm and wait for toast
    await page.locator('.confirm-dialog .action-btn-warn').click();

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 10000 });
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
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Navigate to Packages tab and wait for table rows
    await page.locator('.sidebar-link', { hasText: 'Packages' }).click();
    await page.waitForSelector('.admin-table tbody tr', { timeout: 10000 });

    // Find the package row
    const pkgRow = page.locator('.admin-table tbody tr', {
      hasText: pkgName,
    });
    await expect(pkgRow).toBeVisible();

    // Click Remove — opens confirm dialog
    await pkgRow.locator('button', { hasText: 'Remove' }).click();

    await expect(page.locator('.confirm-dialog')).toBeVisible({ timeout: 10000 });
    await expect(page.locator('.confirm-title')).toHaveText('Remove Package');

    // Confirm and wait for toast
    await page.locator('.confirm-dialog .action-btn-danger').click();

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 10000 });
    const toastText = await page.locator('.toast-success').textContent();
    expect(toastText).toContain('Removed');

    // Package should no longer appear in the list after removal
    await expect(
      page.locator('.admin-table tbody tr', { hasText: pkgName }),
    ).toHaveCount(0, { timeout: 10000 });
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
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Switch to Audit Log tab
    await page.locator('.sidebar-link', { hasText: 'Audit Log' }).click();
    // Wait for tab to switch and data to load
    await page.waitForTimeout(1000);

    // Audit entries should be visible — use the page title as proof tab switched
    await expect(page.locator('h1.page-title', { hasText: 'Audit Log' })).toBeVisible({ timeout: 10000 });

    // Wait for at least one entry to render
    const entries = page.locator('.audit-entry:visible');
    await expect(entries.first()).toBeVisible({ timeout: 10000 });
    const count = await entries.count();
    expect(count).toBeGreaterThanOrEqual(1);
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
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Switch to Reports tab and wait for report content to appear
    await page.locator('.sidebar-link', { hasText: 'Reports' }).click();

    // The report should be visible
    const reportsSection = page.locator('div[x-show*="reports"]');
    await expect(
      reportsSection.locator(`text=${targetPkg}`),
    ).toBeVisible({ timeout: 10000 });
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
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Switch to Reports tab and wait for report content to appear
    await page.locator('.sidebar-link', { hasText: 'Reports' }).click();

    // Find the report and wait for it to be visible
    const reportsSection = page.locator('div[x-show*="reports"]');
    await expect(
      reportsSection.locator(`text=${targetPkg}`),
    ).toBeVisible({ timeout: 10000 });

    // The Dismiss button is next to the report
    await page
      .locator('div[x-show*="reports"]')
      .locator('button', { hasText: 'Dismiss' })
      .first()
      .click();

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 10000 });
    const toastText = await page.locator('.toast-success').textContent();
    expect(toastText).toContain('Dismissed');

    // After dismiss, report should be gone from open list
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

  // ────────────────────────────────────────────
  // 13. Unban user flow
  // ────────────────────────────────────────────
  test('unban user flow', async ({ page, request }) => {
    const adminName = unique('adm-unban');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    // Register target user and ban them via API
    const target = unique('unbannable');
    await api.register(request, target, `${target}@test.com`);
    const targetId = getUserId(target);
    await request.post(`${BASE}/api/v1/admin/users/${targetId}/ban`, {
      headers: { cookie: `session=${adminSession}` },
    });

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Navigate to Users tab
    await page.locator('.sidebar-link', { hasText: 'Users' }).click();
    await page.waitForSelector('.admin-table tbody tr', { timeout: 10000 });

    // Search for the target user
    const searchInput = page.locator(
      'div[x-show*="users"] .toolbar-search',
    );
    await searchInput.fill(target);
    await page.waitForSelector(`.admin-table tbody tr:has-text("${target}")`, { timeout: 10000 });

    // Verify the user shows "Banned" badge
    const userRow = page.locator('.admin-table tbody tr', { hasText: target });
    await expect(userRow).toBeVisible();
    await expect(userRow.locator('.status-banned')).toBeVisible({ timeout: 10000 });

    // Click "Unban" — opens confirm dialog
    await userRow.locator('button', { hasText: 'Unban' }).click();

    // Confirm dialog should appear
    await expect(page.locator('.confirm-dialog')).toBeVisible({ timeout: 10000 });

    // Click the confirm button inside the dialog
    const confirmBtn = page.locator('.confirm-dialog button', { hasText: /Unban|Confirm|Yes/ });
    await confirmBtn.click();

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 10000 });

    // User should no longer show "Banned" badge
    await page.waitForTimeout(500);
    const updatedRow = page.locator('.admin-table tbody tr', { hasText: target });
    await expect(updatedRow.locator('.status-banned')).toHaveCount(0, { timeout: 10000 });
  });

  // ────────────────────────────────────────────
  // 14. Transfer ownership flow
  // ────────────────────────────────────────────
  test('transfer ownership flow', async ({ page, request }) => {
    const adminName = unique('adm-transfer');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    // Register owner and new owner
    const owner = unique('xferowner');
    const ownerSession = await api.register(
      request,
      owner,
      `${owner}@test.com`,
    );
    const newowner = unique('xfernew');
    await api.register(request, newowner, `${newowner}@test.com`);

    // Owner publishes a package
    const ownerToken = await api.createToken(request, ownerSession, 'xfer-tok');
    const pkgName = unique('xferpkg');
    await api.publishPackage(request, ownerToken, pkgName, '1.0.0');

    // Transfer ownership via admin API
    const transferRes = await request.post(
      `${BASE}/api/v1/admin/packages/${pkgName}/transfer`,
      {
        data: { to_username: newowner },
        headers: { cookie: `session=${adminSession}` },
      },
    );
    expect(transferRes.ok()).toBeTruthy();

    // Verify the transfer in the browser on the package detail page
    await api.setSession(page.context(), adminSession);
    await page.goto(`/packages/${pkgName}`);

    // The package detail page should show the new owner
    await expect(page.locator(`text=${newowner}`)).toBeVisible({ timeout: 10000 });
  });

  // ────────────────────────────────────────────
  // 15. Revoke tokens from user drawer
  // ────────────────────────────────────────────
  test('revoke tokens from user drawer', async ({ page, request }) => {
    const adminName = unique('adm-revtok');
    const adminSession = await api.register(
      request,
      adminName,
      `${adminName}@test.com`,
    );
    makeAdmin(adminName);

    // Register target user and create 2 tokens
    const target = unique('tokentarget');
    const targetSession = await api.register(
      request,
      target,
      `${target}@test.com`,
    );
    await api.createToken(request, targetSession, 'tok-a');
    await api.createToken(request, targetSession, 'tok-b');

    await api.setSession(page.context(), adminSession);
    await page.goto('/admin');
    await page.waitForSelector('.stats-grid', { state: 'visible', timeout: 15000 });

    // Navigate to Users tab
    await page.locator('.sidebar-link', { hasText: 'Users' }).click();
    await page.waitForSelector('.admin-table tbody tr', { timeout: 10000 });

    // Search for the target user
    const searchInput = page.locator(
      'div[x-show*="users"] .toolbar-search',
    );
    await searchInput.fill(target);
    await page.waitForSelector(`.admin-table tbody tr:has-text("${target}")`, { timeout: 10000 });

    // Click "View" on the target user to open the drawer
    const userRow = page.locator('.admin-table tbody tr', { hasText: target });
    await expect(userRow).toBeVisible();
    await userRow.locator('button', { hasText: 'View' }).click();

    // Wait for drawer to open and click "Revoke All Tokens"
    await page.locator('button', { hasText: 'Revoke All Tokens' }).click();

    // Toast should show success
    await expect(page.locator('.toast-success')).toBeVisible({ timeout: 10000 });
  });
});
