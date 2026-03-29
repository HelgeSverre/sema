import { test, expect } from './helpers/fixtures';
import {
  register,
  createToken,
  publishPackage,
  addOwner,
  removeOwner,
} from './helpers/api';

test.describe('Owner management', () => {
  test('add owner via API, verify on package detail page', async ({
    authedPage,
    request,
    apiToken,
  }) => {
    const ts = Date.now();
    const name = `own-add-${ts}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    // Register a second user
    const secondUser = `owner2-${ts}`;
    await register(request, secondUser, `${secondUser}@test.com`);

    // Add second user as owner
    await addOwner(request, apiToken, name, secondUser);

    await authedPage.goto(`/packages/${name}`);
    const ownersList = authedPage.getByTestId('owners-list');
    await expect(ownersList).toContainText(secondUser);
  });

  test('remove owner via API, verify removed from page', async ({
    authedPage,
    request,
    apiToken,
  }) => {
    const ts = Date.now();
    const name = `own-rm-${ts}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    // Register and add second user
    const secondUser = `owner-rm-${ts}`;
    await register(request, secondUser, `${secondUser}@test.com`);
    await addOwner(request, apiToken, name, secondUser);

    // Remove second user
    await removeOwner(request, apiToken, name, secondUser);

    await authedPage.goto(`/packages/${name}`);
    const ownersList = authedPage.getByTestId('owners-list');
    await expect(ownersList).not.toContainText(secondUser);
  });

  test('cannot remove last owner', async ({ request, apiToken }) => {
    const ts = Date.now();
    const name = `own-last-${ts}`;
    await publishPackage(request, apiToken, name, '1.0.0');

    // Get the username from the account page via API
    // Try removing the only owner — expect the API to reject it
    const res = await request.delete(
      `http://localhost:3111/api/v1/packages/${name}/owners`,
      {
        data: { username: '__self__' },
        headers: { authorization: `Bearer ${apiToken}` },
      },
    );

    // The server should refuse to remove the last owner
    expect(res.ok()).toBeFalsy();
    const body = await res.text();
    expect(body.toLowerCase()).toMatch(/last owner|cannot remove|at least one/);
  });
});
