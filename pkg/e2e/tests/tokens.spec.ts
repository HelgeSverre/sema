import { test, expect } from './helpers/fixtures';

test.describe('Tokens — API token CRUD lifecycle', () => {
  test('generate token shows token value', async ({ authedPage }) => {
    await authedPage.goto('/account');

    await authedPage.locator('[data-testid="token-name-input"]').fill('my-token');
    await authedPage.locator('[data-testid="token-generate-btn"]').click();

    const display = authedPage.locator('[data-testid="token-display"]');
    await expect(display).toBeVisible();
    await expect(display).toContainText('sema_pat_');
  });

  test('generated token appears in token list', async ({ authedPage }) => {
    await authedPage.goto('/account');

    const tokenName = `list-token-${Date.now()}`;
    await authedPage.locator('[data-testid="token-name-input"]').fill(tokenName);
    await authedPage.locator('[data-testid="token-generate-btn"]').click();

    await expect(authedPage.locator('[data-testid="token-display"]')).toBeVisible();

    const row = authedPage.locator('[data-testid="token-row"]', { hasText: tokenName });
    await expect(row).toBeVisible();
  });

  test('multiple tokens can coexist', async ({ authedPage }) => {
    await authedPage.goto('/account');

    const name1 = `multi-a-${Date.now()}`;
    const name2 = `multi-b-${Date.now()}`;

    await authedPage.locator('[data-testid="token-name-input"]').fill(name1);
    await authedPage.locator('[data-testid="token-generate-btn"]').click();
    await expect(authedPage.locator('[data-testid="token-display"]')).toBeVisible();

    await authedPage.locator('[data-testid="token-name-input"]').fill(name2);
    await authedPage.locator('[data-testid="token-generate-btn"]').click();

    const rows = authedPage.locator('[data-testid="token-row"]');
    // The apiToken fixture also created one, so we check for our two by name
    await expect(rows.filter({ hasText: name1 })).toBeVisible();
    await expect(rows.filter({ hasText: name2 })).toBeVisible();
  });

  test('revoke token removes it from list', async ({ authedPage }) => {
    await authedPage.goto('/account');

    const tokenName = `revoke-token-${Date.now()}`;
    await authedPage.locator('[data-testid="token-name-input"]').fill(tokenName);
    await authedPage.locator('[data-testid="token-generate-btn"]').click();
    await expect(authedPage.locator('[data-testid="token-display"]')).toBeVisible();

    const row = authedPage.locator('[data-testid="token-row"]', { hasText: tokenName });
    await expect(row).toBeVisible();

    // Auto-accept the confirm() dialog before clicking Revoke
    authedPage.on('dialog', (dialog) => dialog.accept());
    await row.getByRole('button', { name: /revoke/i }).click();

    await expect(row).not.toBeVisible();
  });

  test('token name is displayed correctly', async ({ authedPage }) => {
    await authedPage.goto('/account');

    const tokenName = `exact-name-${Date.now()}`;
    await authedPage.locator('[data-testid="token-name-input"]').fill(tokenName);
    await authedPage.locator('[data-testid="token-generate-btn"]').click();
    await expect(authedPage.locator('[data-testid="token-display"]')).toBeVisible();

    const row = authedPage.locator('[data-testid="token-row"]', { hasText: tokenName });
    await expect(row).toBeVisible();
    await expect(row).toContainText(tokenName);
  });
});
