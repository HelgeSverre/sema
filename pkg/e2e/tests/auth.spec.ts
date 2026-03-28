import { test, expect } from '@playwright/test';
import { register } from './helpers/api';

test.describe('Authentication', () => {
  test.describe('Registration', () => {
    test('register with valid credentials ends up on /account', async ({ page }) => {
      const ts = Date.now();
      await page.goto('/login');

      // Switch to the "Create Account" tab
      await page.getByTestId('tab-register').click();

      await page.getByTestId('reg-username').fill(`newuser${ts}`);
      await page.getByTestId('reg-email').fill(`newuser${ts}@test.com`);
      await page.getByTestId('reg-password').fill('password123');
      await page.getByTestId('reg-submit').click();

      await page.waitForURL(/\/account/, { timeout: 10_000 });
      expect(page.url()).toContain('/account');
    });

    test('short username shows validation error', async ({ page }) => {
      await page.goto('/login');
      await page.getByTestId('tab-register').click();

      await page.getByTestId('reg-username').fill('x');
      await page.getByTestId('reg-email').fill('x@test.com');
      await page.getByTestId('reg-password').fill('password123');
      await page.getByTestId('reg-submit').click();

      await expect(page.getByTestId('auth-error')).toBeVisible({ timeout: 5_000 });
    });

    test('short password shows validation error', async ({ page }) => {
      const ts = Date.now();
      await page.goto('/login');
      await page.getByTestId('tab-register').click();

      await page.getByTestId('reg-username').fill(`shortpw${ts}`);
      await page.getByTestId('reg-email').fill(`shortpw${ts}@test.com`);
      await page.getByTestId('reg-password').fill('ab');
      await page.getByTestId('reg-submit').click();

      await expect(page.getByTestId('auth-error')).toBeVisible({ timeout: 5_000 });
    });

    test('bad email shows validation error', async ({ page }) => {
      const ts = Date.now();
      await page.goto('/login');
      await page.getByTestId('tab-register').click();

      await page.getByTestId('reg-username').fill(`bademail${ts}`);
      // Remove type="email" to bypass browser validation, then fill invalid value
      await page.getByTestId('reg-email').evaluate((el: HTMLInputElement) => el.type = 'text');
      await page.getByTestId('reg-email').fill('xx');
      await page.getByTestId('reg-password').fill('password123');
      await page.getByTestId('reg-submit').click();

      await expect(page.getByTestId('auth-error')).toBeVisible({ timeout: 5_000 });
    });

    test('duplicate username shows error', async ({ page, request }) => {
      const ts = Date.now();
      const username = `dupuser${ts}`;
      // Pre-register via API
      await register(request, username, `${username}@test.com`);

      await page.goto('/login');
      await page.getByTestId('tab-register').click();

      await page.getByTestId('reg-username').fill(username);
      await page.getByTestId('reg-email').fill(`${username}x@test.com`);
      await page.getByTestId('reg-password').fill('password123');
      await page.getByTestId('reg-submit').click();

      await expect(page.getByTestId('auth-error')).toBeVisible({ timeout: 5_000 });
    });
  });

  test.describe('Login', () => {
    test('login with valid credentials ends up on /account', async ({ page, request }) => {
      const ts = Date.now();
      const username = `loginuser${ts}`;
      await register(request, username, `${username}@test.com`);

      await page.goto('/login');
      await page.getByTestId('login-username').fill(username);
      await page.getByTestId('login-password').fill('password123');
      await page.getByTestId('login-submit').click();

      await page.waitForURL(/\/account/, { timeout: 10_000 });
      expect(page.url()).toContain('/account');
    });

    test('wrong password shows error', async ({ page, request }) => {
      const ts = Date.now();
      const username = `wrongpw${ts}`;
      await register(request, username, `${username}@test.com`);

      await page.goto('/login');
      await page.getByTestId('login-username').fill(username);
      await page.getByTestId('login-password').fill('totallyWrongPassword');
      await page.getByTestId('login-submit').click();

      await expect(page.getByTestId('auth-error')).toBeVisible({ timeout: 5_000 });
    });

    test('non-existent user shows error', async ({ page }) => {
      await page.goto('/login');
      await page.getByTestId('login-username').fill('nosuchuser999');
      await page.getByTestId('login-password').fill('password123');
      await page.getByTestId('login-submit').click();

      await expect(page.getByTestId('auth-error')).toBeVisible({ timeout: 5_000 });
    });
  });

  test.describe('Session and nav state', () => {
    test('after login, nav shows "Account" link', async ({ page, request }) => {
      const ts = Date.now();
      const username = `navuser${ts}`;
      await register(request, username, `${username}@test.com`);

      await page.goto('/login');
      await page.getByTestId('login-username').fill(username);
      await page.getByTestId('login-password').fill('password123');
      await page.getByTestId('login-submit').click();
      await page.waitForURL(/\/account/, { timeout: 10_000 });

      await expect(page.getByTestId('nav-account')).toBeVisible();
    });

    test('logout clears session and nav reverts to "Sign In"', async ({ page, request }) => {
      const ts = Date.now();
      const username = `logoutuser${ts}`;
      await register(request, username, `${username}@test.com`);

      // Log in via the UI
      await page.goto('/login');
      await page.getByTestId('login-username').fill(username);
      await page.getByTestId('login-password').fill('password123');
      await page.getByTestId('login-submit').click();
      await page.waitForURL(/\/account/, { timeout: 10_000 });

      // Logout via API (POST)
      await page.request.post('/api/v1/auth/logout');

      // Navigate somewhere to see updated nav
      await page.goto('/');
      await expect(page.getByTestId('nav-signin')).toBeVisible();
    });
  });

  test.describe('Tab switching', () => {
    test('tab switching between Sign In and Create Account works', async ({ page }) => {
      await page.goto('/login');

      // Default should show the Sign In form
      await expect(page.getByTestId('login-username')).toBeVisible();

      // Switch to Create Account
      await page.getByTestId('tab-register').click();
      await expect(page.getByTestId('reg-username')).toBeVisible();

      // Switch back to Sign In
      await page.getByTestId('tab-signin').click();
      await expect(page.getByTestId('login-username')).toBeVisible();
    });
  });

  test.describe('Case-insensitive login', () => {
    test('register as "Alice", login as "alice"', async ({ page, request }) => {
      const ts = Date.now();
      const username = `Alice${ts}`;
      await register(request, username, `alice${ts}@test.com`);

      await page.goto('/login');
      await page.getByTestId('login-username').fill(username.toLowerCase());
      await page.getByTestId('login-password').fill('password123');
      await page.getByTestId('login-submit').click();

      await page.waitForURL(/\/account/, { timeout: 10_000 });
      expect(page.url()).toContain('/account');
    });
  });
});
