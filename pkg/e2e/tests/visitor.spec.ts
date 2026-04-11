import { test, expect } from '@playwright/test';
import { register, createToken, publishPackage } from './helpers/api';

test.describe('Visitor (unauthenticated)', () => {
  test('home page loads with heading and search form', async ({ page }) => {
    await page.goto('/');
    await expect(page.getByRole('heading', { name: 'Sema Packages' })).toBeVisible();
    await expect(page.getByTestId('home-search')).toBeVisible();
  });

  test('home page shows recently updated packages', async ({ page, request }) => {
    const session = await register(request, `visitor${Date.now()}a`, `visitor${Date.now()}a@test.com`);
    const token = await createToken(request, session);
    await publishPackage(request, token, `recent-pkg-a-${Date.now()}`, '1.0.0', 'First package');
    await publishPackage(request, token, `recent-pkg-b-${Date.now()}`, '0.2.0', 'Second package');

    await page.goto('/');
    const recentSection = page.getByTestId('recent-packages');
    await expect(recentSection).toBeVisible();
    const items = recentSection.getByTestId('pkg-item');
    await expect(items.first()).toBeVisible();
    expect(await items.count()).toBeGreaterThanOrEqual(2);
  });

  test('search from home page navigates to /search', async ({ page }) => {
    await page.goto('/');
    const searchInput = page.getByTestId('home-search');
    await searchInput.fill('math');
    await searchInput.press('Enter');
    await page.waitForURL(/\/search\?q=math/);
    expect(page.url()).toContain('/search?q=math');
  });

  test('package detail page shows version, install command, and owners', async ({ page, request }) => {
    const ts = Date.now();
    const pkgName = `detail-pkg-${ts}`;
    const session = await register(request, `detailuser${ts}`, `detailuser${ts}@test.com`);
    const token = await createToken(request, session);
    await publishPackage(request, token, pkgName, '2.1.0');

    await page.goto(`/packages/${pkgName}`);
    await expect(page.getByTestId('pkg-name')).toContainText(pkgName);
    await expect(page.getByTestId('install-cmd')).toBeVisible();
    await expect(page.getByTestId('owners-list')).toBeVisible();
  });

  test('nav shows "Sign In" link when not logged in', async ({ page }) => {
    await page.goto('/');
    await expect(page.getByTestId('nav-signin')).toBeVisible();
  });

  test('visiting /account without session redirects to /login', async ({ page }) => {
    await page.goto('/account');
    await page.waitForURL(/\/login/);
    expect(page.url()).toContain('/login');
  });

  test('visiting /link without session redirects to /login', async ({ page }) => {
    await page.goto('/link');
    await page.waitForURL(/\/login/);
    expect(page.url()).toContain('/login');
  });

  test('health endpoint returns ok', async ({ request }) => {
    const res = await request.get('/healthz');
    expect(res.ok()).toBeTruthy();
    expect(await res.text()).toContain('ok');
  });

  test('non-existent package shows 404 or error', async ({ page }) => {
    const res = await page.goto('/packages/this-package-does-not-exist-999');
    // Either a 404 status or an error/empty page
    expect(res).not.toBeNull();
    const status = res!.status();
    if (status === 404) {
      expect(status).toBe(404);
    } else {
      // Page loaded but should indicate package not found
      const body = await page.textContent('body');
      expect(body?.toLowerCase()).toMatch(/not found|404|no package/);
    }
  });
});
