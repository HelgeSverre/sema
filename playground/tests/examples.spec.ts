import { test, expect, Page } from '@playwright/test';

// Example file names as they appear in the sidebar tree
const EXAMPLES = [
  'hello.sema',
  'fibonacci.sema',
  'fizzbuzz.sema',
  'quicksort.sema',
  'closures.sema',
  'map-filter.sema',
  'strings.sema',
  'macros.sema',
  'maze.sema',
  'mandelbrot.sema',
  'perlin-noise.sema',
  'game-of-life.sema',
  'ascii-art.sema',
];

/** Wait for the WASM module to be ready. */
async function waitForReady(page: Page) {
  await page.goto('/');
  await page.waitForSelector('[data-testid="status"].status-ready', { timeout: 15000 });
}

/** Type code into the editor, replacing existing content. */
async function setEditorCode(page: Page, code: string) {
  await page.getByTestId('editor').fill(code);
}

/** Click Run and wait for the timing line to appear. */
async function clickRunAndWait(page: Page) {
  await page.getByTestId('run-btn').click();
  await page.waitForSelector('#output .output-timing', { timeout: 30000 });
}

/** Select the VM engine. */
async function selectVM(page: Page) {
  await page.getByTestId('engine-vm').click();
}

/** Select the tree-walker engine. */
async function selectTree(page: Page) {
  await page.getByTestId('engine-tree').click();
}

test.beforeEach(async ({ page }) => {
  await waitForReady(page);
});

// ── Example smoke tests (tree-walker) ──

for (const name of EXAMPLES) {
  test(`example: ${name}`, async ({ page }) => {
    // Click the example button in the sidebar tree
    await page.click(`.tree-file:text("${name}")`);

    // Verify editor has content
    const editorValue = await page.getByTestId('editor').inputValue();
    expect(editorValue.length).toBeGreaterThan(10);

    // Click Run
    await clickRunAndWait(page);

    // Check there's no error
    const errorEl = await page.$('#output .output-error');
    if (errorEl) {
      const errorText = await errorEl.textContent();
      throw new Error(`Example "${name}" produced error: ${errorText}`);
    }

    // Check we got some output (either output lines or a value)
    const outputLines = await page.$$('#output .output-line');
    const valueLines = await page.$$('#output .output-value');
    expect(outputLines.length + valueLines.length).toBeGreaterThan(0);

    // Verify timing shows tree-walker
    const timing = await page.$eval('#output .output-timing', el => el.textContent);
    expect(timing).toContain('tree-walker');
  });
}

test('whitespace preserved in output', async ({ page }) => {
  // Use the Maze example which relies on whitespace alignment
  await page.click('.tree-file:text("maze.sema")');
  await clickRunAndWait(page);

  // Check that output lines have white-space: pre
  const style = await page.$eval('.output-line', (el) =>
    window.getComputedStyle(el).whiteSpace
  );
  expect(style).toBe('pre');
});

// ── VM engine toggle tests ──

test('vm toggle: runs code with bytecode VM', async ({ page }) => {
  await setEditorCode(page, '(+ 1 2)');
  await selectVM(page);
  await clickRunAndWait(page);

  // Check result
  const value = await page.$eval('#output .output-value', el => el.textContent);
  expect(value).toContain('3');

  // Verify timing shows bytecode VM
  const timing = await page.$eval('#output .output-timing', el => el.textContent);
  expect(timing).toContain('bytecode VM');
});

test('vm toggle: switching back to tree-walker works', async ({ page }) => {
  await setEditorCode(page, '(* 6 7)');

  // Run with VM first
  await selectVM(page);
  await clickRunAndWait(page);
  let timing = await page.$eval('#output .output-timing', el => el.textContent);
  expect(timing).toContain('bytecode VM');

  // Switch back to tree-walker
  await selectTree(page);
  await clickRunAndWait(page);
  timing = await page.$eval('#output .output-timing', el => el.textContent);
  expect(timing).toContain('tree-walker');
});

test('vm toggle: tree and vm produce same result', async ({ page }) => {
  const code = `(define (fib n)
  (define (go a b i)
    (if (= i 0) a (go b (+ a b) (- i 1))))
  (go 0 1 n))
(fib 20)`;

  // Run with tree-walker
  await selectTree(page);
  await setEditorCode(page, code);
  await clickRunAndWait(page);
  const treeValue = await page.$eval('#output .output-value', el => el.textContent);

  // Run with VM
  await selectVM(page);
  await clickRunAndWait(page);
  const vmValue = await page.$eval('#output .output-value', el => el.textContent);

  expect(treeValue).toBe(vmValue);
});

test('vm toggle: active class updates on toggle', async ({ page }) => {
  // Tree should be active by default
  const treeLabel = page.getByTestId('engine-tree');
  const vmLabel = page.getByTestId('engine-vm');
  await expect(treeLabel).toHaveClass(/active/);
  await expect(vmLabel).not.toHaveClass(/active/);

  // Click VM
  await selectVM(page);
  await expect(vmLabel).toHaveClass(/active/);
  await expect(treeLabel).not.toHaveClass(/active/);

  // Click Tree
  await selectTree(page);
  await expect(treeLabel).toHaveClass(/active/);
  await expect(vmLabel).not.toHaveClass(/active/);
});

test('vm toggle: hello.sema works in VM mode', async ({ page }) => {
  await page.click('.tree-file:text("hello.sema")');
  await selectVM(page);
  await clickRunAndWait(page);

  const errorEl = await page.$('#output .output-error');
  expect(errorEl).toBeNull();

  const value = await page.$eval('#output .output-value', el => el.textContent);
  expect(value).toContain('Hello, world!');
});

test('clear button clears output', async ({ page }) => {
  await setEditorCode(page, '(println "test")');
  await clickRunAndWait(page);

  // Output should have content
  const before = await page.getByTestId('output').innerHTML();
  expect(before.length).toBeGreaterThan(0);

  // Clear
  await page.getByTestId('clear-btn').click();
  const after = await page.getByTestId('output').innerHTML();
  expect(after).toBe('');
});
