import { test as base, type Page, type BrowserContext } from '@playwright/test';
import * as api from './api';

/** Fixtures for authenticated and seeded test scenarios. */
type Fixtures = {
  /** A browser context with a logged-in session. */
  authedContext: BrowserContext;
  /** A page from the authenticated context. */
  authedPage: Page;
  /** Session cookie string for API calls. */
  session: string;
  /** API token for publishing. */
  apiToken: string;
};

let userCounter = 0;

export const test = base.extend<Fixtures>({
  session: async ({ request }, use) => {
    userCounter++;
    const username = `e2euser${userCounter}${Date.now()}`;
    const email = `${username}@test.com`;
    const session = await api.register(request, username, email);
    await use(session);
  },

  authedContext: async ({ browser, session }, use) => {
    const context = await browser.newContext();
    await api.setSession(context, session);
    await use(context);
    await context.close();
  },

  authedPage: async ({ authedContext }, use) => {
    const page = await authedContext.newPage();
    await use(page);
  },

  apiToken: async ({ request, session }, use) => {
    const token = await api.createToken(request, session, 'e2e-token');
    await use(token);
  },
});

export { expect } from '@playwright/test';
