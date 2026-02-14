import { test, expect } from '@playwright/test';

const EXAMPLES = [
  'Hello',
  'Fibonacci',
  'Map & Filter',
  'FizzBuzz',
  'Closures',
  'Strings',
  'Macros',
  'Quicksort',
  'Maze',
  'Mandelbrot',
  'Perlin Noise',
  'Game of Life',
  'ASCII Art',
];

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  // Wait for WASM to load
  await page.waitForSelector('#status.status-ready', { timeout: 15000 });
});

for (const name of EXAMPLES) {
  test(`example: ${name}`, async ({ page }) => {
    // Click the example button
    await page.click(`.example-btn:text("${name}")`);

    // Verify editor has content
    const editorValue = await page.inputValue('#editor');
    expect(editorValue.length).toBeGreaterThan(10);

    // Click Run
    await page.click('#run-btn');

    // Wait for output to appear
    await page.waitForSelector('#output .output-timing', { timeout: 30000 });

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
  });
}

test('whitespace preserved in output', async ({ page }) => {
  // Use the Maze example which relies on whitespace alignment
  await page.click('.example-btn:text("Maze")');
  await page.click('#run-btn');
  await page.waitForSelector('#output .output-timing', { timeout: 30000 });

  // Check that output lines have white-space: pre
  const style = await page.$eval('.output-line', (el) =>
    window.getComputedStyle(el).whiteSpace
  );
  expect(style).toBe('pre');
});
