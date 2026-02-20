import { test, expect, Page } from '@playwright/test';

async function waitForReady(page: Page) {
  await page.goto('/vfs-demo/');
  await page.waitForSelector('#status-text.status-ready', { timeout: 15000 });
}

async function clickRun(page: Page) {
  await page.getByTestId('run-btn').click();
  // Wait for status to go back to Ready
  await page.waitForSelector('#status-text.status-ready', { timeout: 30000 });
}

test('VFS demo: run script and check file tree has nested files', async ({ page }) => {
  // Capture console output for debugging
  const logs: string[] = [];
  page.on('console', msg => logs.push(`[${msg.type()}] ${msg.text()}`));

  await waitForReady(page);
  await clickRun(page);

  // Check output has content
  const output = page.getByTestId('output');
  await expect(output).not.toBeEmpty();
  const outputText = await output.innerText();
  console.log('=== OUTPUT ===');
  console.log(outputText);

  // Check file tree
  const fileTree = page.getByTestId('file-tree');
  const treeHtml = await fileTree.innerHTML();
  console.log('=== FILE TREE HTML ===');
  console.log(treeHtml);

  // Check what the tree contains text-wise
  const treeText = await fileTree.innerText();
  console.log('=== FILE TREE TEXT ===');
  console.log(treeText);

  // Dump JS-side VFS state via page.evaluate
  const vfsState = await page.evaluate(() => {
    // @ts-ignore - interp is on window scope via module, need to access differently
    // Let's use the WASM directly
    return {
      treeChildrenCount: document.querySelectorAll('#file-tree .tree-dir').length,
      treeFileCount: document.querySelectorAll('#file-tree .tree-file').length,
      allElements: Array.from(document.querySelectorAll('#file-tree *')).map(el => ({
        tag: el.tagName,
        className: el.className,
        text: el.textContent?.slice(0, 80),
        display: getComputedStyle(el).display,
      })),
    };
  });
  console.log('=== VFS DOM STATE ===');
  console.log(JSON.stringify(vfsState, null, 2));

  // Print console logs
  console.log('=== CONSOLE LOGS ===');
  for (const log of logs) console.log(log);

  // Basic assertions
  expect(vfsState.treeChildrenCount).toBeGreaterThan(0);
  // We expect files inside subdirs (index.md, about-us.md, etc.)
  expect(vfsState.treeFileCount).toBeGreaterThan(0);
});
