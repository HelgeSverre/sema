import { test, expect, Page } from '@playwright/test';

// E2E for the `sema workflow view` dashboard — the full variant-5b three-pane
// layout — rendering the rich `audit-auth` fixture journal (4 phases, 4 agents
// with models + per-agent budget, tool calls, checkpoints; run still live).

async function open(page: Page) {
  await page.goto('/?run=audit-auth', { waitUntil: 'networkidle' });
  await page.waitForSelector('[data-testid="phase"]', { timeout: 15000 });
}

test('header + rollup: name, live status, phases/agents/tokens/cost', async ({ page }) => {
  await open(page);
  await expect(page.locator('#wfname')).toHaveText('audit-auth');
  await expect(page.locator('#status-pill')).toHaveText('running'); // no run.ended → live
  await expect(page.locator('#r-phases')).toHaveText('4');
  await expect(page.locator('#r-agents')).toHaveText('4');
  await expect(page.locator('#r-tokens')).toHaveText('9.4k'); // 4000+3740+1660
  await expect(page.locator('#r-cost')).toContainText('0.0058'); // 0.0041 + 0.0017 (auditor_2 unpriced)
  // meta strip is populated from run.started
  await expect(page.locator('#m-runid')).toHaveText('wf_audit_auth_8f3a21');
  await expect(page.locator('#m-code')).toHaveText('a3f1c09e');
});

test('left pane: the phase ledger renders all four phases in order', async ({ page }) => {
  await open(page);
  const phases = page.locator('[data-testid="phase"]');
  await expect(phases).toHaveCount(4);
  for (const name of ['inventory', 'audit', 'verify', 'report']) {
    await expect(page.locator(`[data-testid="phase"][data-phase-name="${name}"]`)).toHaveCount(1);
  }
});

test('right pane: the raw event stream renders one row per journal event', async ({ page }) => {
  await open(page);
  // 24 events in the fixture → 24 stream rows; the cursor shows the last seq.
  await expect(page.locator('[data-testid="ev-row"]')).toHaveCount(24);
  await expect(page.locator('#stream-cursor')).toHaveText('23');
});

test('center pane: selecting the audit phase shows its 3 agents with model + columns', async ({ page }) => {
  await open(page);
  await page.locator('[data-testid="phase"][data-phase-name="audit"]').click();
  const agents = page.locator('[data-testid="agent-row"]');
  await expect(agents).toHaveCount(3);
  // model column is rendered (full model id, per the prototype)
  await expect(page.locator('.amodel').first()).toContainText('claude-haiku');
  // one auditor failed → a failed status row exists
  await expect(page.locator('[data-testid="agent-row"][data-status="failed"]')).toHaveCount(1);
});

test('drill-in details: Prompt / Tool calls / Output digest sections', async ({ page }) => {
  await open(page);
  await page.locator('[data-testid="phase"][data-phase-name="audit"]').click();
  await page.locator('[data-testid="agent-row"]').first().click();
  const drill = page.locator('[data-testid="drill"]');
  await expect(drill).toBeVisible();
  await expect(drill).toContainText('Prompt');
  await expect(drill).toContainText('Tool calls');
  await expect(drill).toContainText('Output');
});

test('status pill is on-brand (rounded, brand pill radius)', async ({ page }) => {
  await open(page);
  const radius = await page
    .locator('#status-pill')
    .evaluate((el) => getComputedStyle(el).borderRadius);
  expect(parseFloat(radius)).toBeGreaterThanOrEqual(12); // brand "pill" = 20px, not square
});

test('event stream: finished agents do not pulse; only a live agent does', async ({ page }) => {
  await open(page);
  // auditor_1 finished (has agent.result) → its agent.started stream glyph must NOT pulse.
  const finishedGlyph = page.locator('[data-testid="ev-row"][data-ev-agent="auditor_1"] .eg').first();
  await expect(finishedGlyph).not.toHaveClass(/pulse/);
  // reporter_1 is still running (no agent.result) → its glyph DOES pulse.
  const liveGlyph = page.locator('[data-testid="ev-row"][data-ev-agent="reporter_1"] .eg').first();
  await expect(liveGlyph).toHaveClass(/pulse/);
});

test('phase ledger: checkpoint-only phase shows ◇N, not a misleading 0/0', async ({ page }) => {
  await open(page);
  // verify is a pure-checkpoint phase (1 checkpoint, 0 agents) → "◇1", never "0/0".
  const verifyCount = page.locator('[data-testid="phase"][data-phase-name="verify"] [data-testid="phase-count"]');
  await expect(verifyCount).toHaveText('◇1');
  await expect(verifyCount).not.toHaveText('0/0');
  // audit has agents → still the done/total form.
  await expect(
    page.locator('[data-testid="phase"][data-phase-name="audit"] [data-testid="phase-count"]')
  ).toHaveText('3/3');
});

test('event stream → click jumps to the agent in the detail pane', async ({ page }) => {
  await open(page);
  // The agent.result for auditor_2 (an event-stream row) jumps to auditor_2.
  const row = page.locator('[data-testid="ev-row"][data-ev-agent="auditor_2"]').first();
  await row.click();
  await expect(page.locator('[data-testid="agent-row"][data-agent="auditor_2"].sel')).toHaveCount(1);
});
