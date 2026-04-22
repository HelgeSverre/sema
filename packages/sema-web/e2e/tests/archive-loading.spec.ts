import { execFileSync } from "node:child_process";
import { mkdirSync, rmSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { test, expect } from "@playwright/test";
import { waitForSema } from "../helpers";

test.setTimeout(120_000);
test.describe.configure({ mode: "serial" });

const testDir = path.dirname(fileURLToPath(import.meta.url));
const fixtureDir = path.resolve(testDir, "../fixtures");
const repoRoot = path.resolve(testDir, "../../../..");
const archiveDir = path.join(fixtureDir, "generated");
const basicArchivePath = path.join(archiveDir, "basic.vfs");
const basicEntryPath = path.join(fixtureDir, "scripts/basic.sema");
const counterArchivePath = path.join(archiveDir, "counter-component.vfs");
const counterEntryPath = path.join(fixtureDir, "scripts/counter.sema");

function buildArchive(entryPath: string, archivePath: string): void {
  execFileSync(
    "cargo",
    [
      "run",
      "-p",
      "sema-lang",
      "--",
      "build",
      "--target",
      "web",
      entryPath,
      "-o",
      archivePath,
    ],
    {
      cwd: repoRoot,
      stdio: "pipe",
      maxBuffer: 10 * 1024 * 1024,
    }
  );
}

test.beforeAll(() => {
  mkdirSync(archiveDir, { recursive: true });
  buildArchive(basicEntryPath, basicArchivePath);
  buildArchive(counterEntryPath, counterArchivePath);
});

test.afterAll(() => {
  rmSync(basicArchivePath, { force: true });
  rmSync(counterArchivePath, { force: true });
});

test("built .vfs archives load and execute in the browser", async ({ page }) => {
  await page.goto("/archive-basic.html");
  await waitForSema(page);
  await expect(page.locator("#app")).toHaveText("Hello from compiled Sema!");
});

test("built .vfs archives support defcomponent and mount!", async ({ page }) => {
  await page.goto("/archive-counter.html");
  await waitForSema(page);

  const display = page.locator("#count-display");
  await expect(display).toHaveText("0");

  await page.click("#btn-inc");
  await page.click("#btn-inc");
  await expect(display).toHaveText("2");

  await page.click("#btn-dec");
  await expect(display).toHaveText("1");

  await page.click("#btn-reset");
  await expect(display).toHaveText("0");
});
