import { test, expect } from '@playwright/test';
import { register, createToken, setSession } from './helpers/api';

test.describe('GitHub integration (unconnected state)', () => {
  test('account page shows "Connect GitHub" when not connected', async ({
    page,
    request,
    browser,
  }) => {
    const ts = Date.now();
    const username = `gh-acct-${ts}`;
    const session = await register(request, username, `${username}@test.com`);

    const context = await browser.newContext();
    await setSession(context, session);
    const authed = await context.newPage();

    await authed.goto('/account');
    const githubSection = authed.getByTestId('section-github');
    await expect(githubSection).toBeVisible();
    await expect(githubSection).toContainText('Connect GitHub');

    await context.close();
  });

  test('link page shows "Connect GitHub" prompt when not connected', async ({
    page,
    request,
    browser,
  }) => {
    const ts = Date.now();
    const username = `gh-link-${ts}`;
    const session = await register(request, username, `${username}@test.com`);

    const context = await browser.newContext();
    await setSession(context, session);
    const authed = await context.newPage();

    await authed.goto('/link');
    await expect(authed.locator('body')).toContainText('Connect GitHub');

    await context.close();
  });

  test.skip('GitHub badge on package detail for linked package', async () => {
    // Cannot seed a GitHub-linked package without real OAuth flow.
    // When a direct DB seeding endpoint or test helper is available,
    // verify that data-testid="github-badge" is visible on the
    // package detail page for a GitHub-linked package.
  });
});
