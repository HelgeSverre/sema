import { test, expect } from './helpers/fixtures';
import { publishPackage, yankVersion } from './helpers/api';

test.describe('Publisher — publish packages via API, verify on web UI', () => {
  test('published package appears on home page', async ({ authedPage, request, apiToken }) => {
    const name = `pub-home-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto('/');
    const recent = authedPage.locator('[data-testid="recent-packages"]');
    await expect(recent).toContainText(name);
  });

  test('package detail page shows version and description', async ({
    authedPage,
    request,
    apiToken,
  }) => {
    const name = `pub-detail-${Date.now()}`;
    const description = 'A detailed test package';
    await publishPackage(request, apiToken, name, '1.0.0', description);

    await authedPage.goto(`/packages/${name}`);
    await expect(authedPage.locator('[data-testid="pkg-name"]')).toHaveText(name);
    await expect(authedPage.locator('[data-testid="versions-table"]')).toContainText('1.0.0');
    await expect(authedPage.locator('body')).toContainText(description);
  });

  test('publishing multiple versions shows all in version table', async ({
    authedPage,
    request,
    apiToken,
  }) => {
    const name = `pub-multi-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');
    await publishPackage(request, apiToken, name, '2.0.0');

    await authedPage.goto(`/packages/${name}`);
    const table = authedPage.locator('[data-testid="versions-table"]');
    await expect(table).toContainText('1.0.0');
    await expect(table).toContainText('2.0.0');
  });

  test('My Packages on account page lists owned packages', async ({
    authedPage,
    request,
    apiToken,
  }) => {
    const name = `pub-account-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto('/account');
    const section = authedPage.locator('[data-testid="section-packages"]');
    await expect(section).toContainText(name);
  });

  test('yanked version shows "yanked" indicator', async ({ authedPage, request, apiToken }) => {
    const name = `pub-yank-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');
    await yankVersion(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/packages/${name}`);
    const table = authedPage.locator('[data-testid="versions-table"]');
    await expect(table).toContainText('yanked');
  });
});
