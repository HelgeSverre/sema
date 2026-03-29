import { test, expect } from './helpers/fixtures';
import { publishPackage } from './helpers/api';

test.describe('Search and pagination', () => {
  test('search from header input navigates to results', async ({
    authedPage,
    request,
    apiToken,
  }) => {
    const name = `hdr-search-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/packages/${name}`);
    const headerSearch = authedPage.locator('header form[action="/search"] input[name="q"]');
    await headerSearch.fill(name);
    await headerSearch.press('Enter');

    await authedPage.waitForURL(/\/search\?q=/);
    expect(authedPage.url()).toContain('/search?q=');
  });

  test('search results match query', async ({ authedPage, request, apiToken }) => {
    const ts = Date.now();
    const httpPkg = `http-client-${ts}`;
    const jsonPkg = `json-parser-${ts}`;
    await publishPackage(request, apiToken, httpPkg, '1.0.0');
    await publishPackage(request, apiToken, jsonPkg, '1.0.0');

    await authedPage.goto(`/search?q=http-client-${ts}`);
    const results = authedPage.getByTestId('search-results');
    await expect(results).toContainText(httpPkg);
    await expect(results).not.toContainText(jsonPkg);
  });

  test('no results state', async ({ authedPage }) => {
    await authedPage.goto('/search?q=nonexistent-xyz-999');
    await expect(authedPage.locator('body')).toContainText('No packages found');
  });

  test('pagination appears when more than 20 results', async ({
    authedPage,
    request,
    apiToken,
  }) => {
    const ts = Date.now();
    // Publish 25 packages to exceed the per_page=20 default
    const publishes = [];
    for (let i = 0; i < 25; i++) {
      publishes.push(
        publishPackage(request, apiToken, `bulk-pkg-${i}-${ts}`, '1.0.0'),
      );
    }
    await Promise.all(publishes);

    await authedPage.goto(`/search?q=bulk-pkg`);
    const pagination = authedPage.getByTestId('pagination');
    await expect(pagination).toBeVisible();
    await expect(pagination).toContainText('Next');
  });

  test('direct URL /search?q=foo&page=1 works', async ({ authedPage, request, apiToken }) => {
    const name = `direct-url-${Date.now()}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    await authedPage.goto(`/search?q=${name}&page=1`);
    const results = authedPage.getByTestId('search-results');
    await expect(results).toContainText(name);
  });

  test('results count is displayed', async ({ authedPage, request, apiToken }) => {
    const ts = Date.now();
    await publishPackage(request, apiToken, `count-pkg-a-${ts}`, '1.0.0');
    await publishPackage(request, apiToken, `count-pkg-b-${ts}`, '1.0.0');

    await authedPage.goto(`/search?q=count-pkg`);
    const count = authedPage.getByTestId('results-count');
    await expect(count).toBeVisible();
    const text = await count.textContent();
    const num = parseInt(text!.replace(/\D/g, ''), 10);
    expect(num).toBeGreaterThanOrEqual(2);
  });
});
