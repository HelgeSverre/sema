import { test, expect, Page } from '@playwright/test';

// E2E gate (Slice 2): breakpoints INSIDE async tasks STOP + CONTINUE in the
// cooperative WASM playground debugger. Modeled on debugger.spec.ts — same UI
// flow + selectors. We inject our OWN known programs (not a built-in example) so
// the breakpoint line is deterministic.

async function waitForReady(page: Page) {
  await page.goto('/');
  await page.waitForSelector('[data-testid="status"].status-ready', { timeout: 15000 });
}

async function setEditorCode(page: Page, code: string) {
  await page.getByTestId('editor').fill(code);
}

/** Click a gutter line number to toggle a breakpoint. */
async function toggleBreakpoint(page: Page, lineNum: number) {
  await page.click(`.gutter-line:nth-child(${lineNum})`);
}

/** Get the current line the debugger highlights. */
async function getCurrentDebugLine(page: Page): Promise<number | null> {
  const el = await page.$('.gutter-line.current-line');
  if (!el) return null;
  const text = await el.textContent();
  return text ? parseInt(text, 10) : null;
}

/** Get all error output. */
async function getErrors(page: Page): Promise<string[]> {
  return page.$$eval('#output .output-error', els => els.map(el => el.textContent ?? ''));
}

/** Wait for the debugger to pause (status bar shows "Paused at line ..."). */
async function waitForPaused(page: Page, timeout = 8000) {
  await page.waitForFunction(
    () => document.getElementById('status')?.textContent?.startsWith('Paused'),
    { timeout }
  );
}

/** Wait for the debugger to return to idle (run finished). */
async function waitForIdle(page: Page, timeout = 12000) {
  await page.waitForFunction(
    () => document.getElementById('status')?.textContent === 'Ready',
    { timeout }
  );
}

test.describe('Async debugger (cooperative WASM)', () => {
  test.beforeEach(async ({ page }) => {
    await waitForReady(page);
  });

  test('breakpoint inside an async task: stops on the task line, then Continue finishes', async ({
    page,
  }) => {
    // Line 2 is `(+ 1 2)` — runs ONLY inside the spawned task body. Before
    // Slice 2 the cooperative debugger swallowed this stop and ran to the end.
    const code = '(define p (async/spawn (fn ()\n  (+ 1 2))))\n(await p)';
    await setEditorCode(page, code);

    // Set the breakpoint on line 2 and start debugging.
    await toggleBreakpoint(page, 2);
    await page.getByTestId('debug-btn').click();

    // Must pause INSIDE the task, on line 2.
    await waitForPaused(page);
    expect(await getCurrentDebugLine(page)).toBe(2);

    // Continue → the task + the await must run to completion.
    await page.click('#dbg-continue');
    await waitForIdle(page);

    expect((await getErrors(page)).join('\n')).not.toContain('scheduler');
  });

  test('breakpoint inside the second of two async tasks: pauses at the known line', async ({
    page,
  }) => {
    // 1  (define a (async/spawn (fn ()
    // 2    (* 2 3))))
    // 3  (define b (async/spawn (fn ()
    // 4    (+ 10 20))))      <- breakpoint here, inside task b only
    // 5  (async/all (list a b))
    const code =
      '(define a (async/spawn (fn ()\n  (* 2 3))))\n' +
      '(define b (async/spawn (fn ()\n  (+ 10 20))))\n' +
      '(async/all (list a b))';
    await setEditorCode(page, code);

    await toggleBreakpoint(page, 4);
    await page.getByTestId('debug-btn').click();

    await waitForPaused(page);
    // The stop must be on line 4 (task b's body), not line 2 (task a) or the
    // top-level async/all.
    expect(await getCurrentDebugLine(page)).toBe(4);

    await page.click('#dbg-continue');
    await waitForIdle(page);

    expect((await getErrors(page)).join('\n')).not.toContain('scheduler');
  });
});
