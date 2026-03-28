import { test, expect } from './helpers/fixtures';
import { publishPackage, yankVersion } from './helpers/api';

test.describe('Package detail page', () => {
  test('versions tab is default active', async ({ authedPage, request, apiToken }) => {
    const name = `detail-tabs-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/packages/${name}`);
    const versionsTab = authedPage.getByTestId('tab-versions');
    await expect(versionsTab).toHaveClass(/active/);
    await expect(authedPage.getByTestId('versions-table')).toBeVisible();
  });

  test('switching to dependencies tab works', async ({ authedPage, request, apiToken }) => {
    const name = `detail-deps-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/packages/${name}`);
    await authedPage.getByTestId('tab-deps').click();

    await expect(authedPage.locator('#deps')).toBeVisible();
    await expect(authedPage.getByTestId('versions-table')).toBeHidden();
  });

  test('install command shows correct package name', async ({ authedPage, request, apiToken }) => {
    const name = `detail-install-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/packages/${name}`);
    await expect(authedPage.getByTestId('install-cmd')).toContainText(name);
  });

  test('sidebar shows owners', async ({ authedPage, request, apiToken }) => {
    const name = `detail-owners-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/packages/${name}`);
    const ownersList = authedPage.getByTestId('owners-list');
    await expect(ownersList).toBeVisible();
    // Should have at least one owner (the publisher)
    await expect(ownersList.locator('.sidebar-value')).toHaveCount(1);
  });

  test('multiple versions sort newest first', async ({ authedPage, request, apiToken }) => {
    const name = `detail-sort-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');
    await publishPackage(request, apiToken, name, '2.0.0');

    await authedPage.goto(`/packages/${name}`);
    const table = authedPage.getByTestId('versions-table');
    await expect(table).toBeVisible();

    // Get all version cells and verify v2.0.0 appears before v1.0.0
    const tableText = await table.textContent();
    const idx200 = tableText!.indexOf('2.0.0');
    const idx100 = tableText!.indexOf('1.0.0');
    expect(idx200).toBeGreaterThanOrEqual(0);
    expect(idx100).toBeGreaterThanOrEqual(0);
    expect(idx200).toBeLessThan(idx100);
  });

  test('yanked versions show visual indicator', async ({ authedPage, request, apiToken }) => {
    const name = `detail-yanked-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');
    await yankVersion(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/packages/${name}`);
    const table = authedPage.getByTestId('versions-table');
    await expect(table).toContainText('yanked');
  });
});
