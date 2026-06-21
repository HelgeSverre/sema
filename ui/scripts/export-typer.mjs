#!/usr/bin/env node
// Export <sema-code-typer> to an animated GIF or WebP.
//
// Drives the *real* component (built standalone bundle) headlessly via its deterministic
// seek() API — so the export is pixel-identical to what ships in the browser. One typer,
// one look, many outputs.
//
//   node scripts/export-typer.mjs --input ../examples/maze.sema --out maze.gif \
//     --frame --status --line-numbers --logo --rows 16 --cps 45 --fps 24 --width 900
//
// Output format is inferred from --out extension (.gif | .webp). Build first: `npm run build`.

import { readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, resolve, extname } from 'node:path';
import { chromium } from 'playwright';
import sharp from 'sharp';
import gifenc from 'gifenc';

const { GIFEncoder, quantize, applyPalette } = gifenc;
const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');

function parseArgs(argv) {
  const a = {
    input: '', out: '', filename: '', format: '',
    frames: 140, fps: 18, width: 900, rows: 0,
    frame: false, status: false, lineNumbers: false, logo: false,
  };
  for (let i = 0; i < argv.length; i++) {
    const k = argv[i];
    const next = () => argv[++i];
    if (k === '--input') a.input = next();
    else if (k === '--out') a.out = next();
    else if (k === '--frames') a.frames = Number(next());
    else if (k === '--fps') a.fps = Number(next());
    else if (k === '--width') a.width = Number(next());
    else if (k === '--rows') a.rows = Number(next());
    else if (k === '--filename') a.filename = next();
    else if (k === '--frame') a.frame = true;
    else if (k === '--status') a.status = true;
    else if (k === '--line-numbers') a.lineNumbers = true;
    else if (k === '--logo') a.logo = true;
  }
  if (!a.input || !a.out) {
    console.error('usage: export-typer.mjs --input <file.sema> --out <file.gif|file.webp> [options]');
    process.exit(1);
  }
  a.format = extname(a.out).slice(1).toLowerCase();
  a.filename ??= a.input.split('/').pop();
  return a;
}

// Sema brand tokens so colors match the playground/brand page.
const TOKENS = `
  --bg:#131110; --text-primary:#e9e3d6; --text-secondary:#968c79; --text-tertiary:#6b6354;
  --border:#2b2620; --gold:#c8a855; --radius-lg:8px;
  --mono:'DejaVu Sans Mono',ui-monospace,Menlo,Consolas,monospace;
  --syntax-comment:#6b6354; --syntax-keyword:#c8a855; --syntax-string:#a8c47a;
  --syntax-number:#d19a66; --syntax-keyword-lit:#7aacb8; --syntax-punctuation:#6a6258;
  --syntax-builtin:#c8a855; --syntax-function:#c8a855; --syntax-variable:#e9e3d6;`;

const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
const attr = (a) =>
  [a.frame && 'frame', a.status && 'status', a.lineNumbers && 'line-numbers', a.logo && 'logo']
    .filter(Boolean)
    .join(' ');

async function encodeGifBuffer(frames, delayMs) {
  const enc = GIFEncoder();
  for (const buf of frames) {
    const { data, info } = await sharp(buf).ensureAlpha().raw().toBuffer({ resolveWithObject: true });
    const palette = quantize(data, 256, { format: 'rgba4444' });
    const index = applyPalette(data, palette, 'rgba4444');
    enc.writeFrame(index, info.width, info.height, { palette, delay: delayMs });
  }
  enc.finish();
  return Buffer.from(enc.bytes());
}

async function main() {
  const a = parseArgs(process.argv.slice(2));
  const code = await readFile(resolve(process.cwd(), a.input), 'utf8');
  const bundle = await readFile(resolve(root, 'dist/sema-ui.js'), 'utf8').catch(() => {
    console.error('dist/sema-ui.js not found — run `npm run build` first.');
    process.exit(1);
  });

  const html = `<!doctype html><html><head><meta charset="utf-8"><style>
    :root{${TOKENS}} html,body{margin:0;background:var(--bg)}
    #host{display:block;width:${a.width}px;padding:16px}</style>
    <script type="module">${bundle}</script></head>
    <body><div id="host"><sema-code-typer id="t" ${attr(a)} ${a.rows ? `rows="${a.rows}"` : ''}
      filename="${esc(a.filename)}" autoplay="false">${esc(code)}</sema-code-typer></div></body></html>`;

  const browser = await chromium.launch();
  const page = await browser.newPage({ deviceScaleFactor: 2 });
  await page.setContent(html, { waitUntil: 'networkidle' });
  await page.waitForFunction(() => customElements.get('sema-code-typer'));
  const el = await page.$('#t');

  // Wait for tokenization, then read the real (post-dedent) length from the component.
  await page.waitForFunction(() => {
    const t = document.getElementById('t');
    if (!t || !t.total) return false;
    t.seek(t.total);
    return (t.shadowRoot.querySelector('.code')?.textContent?.length ?? 0) > 0;
  });
  const total = await page.evaluate(() => document.getElementById('t').total);

  // Frame count is fixed (--frames), independent of playback rate (--fps), so output size
  // stays bounded regardless of file length. fps only sets the per-frame delay.
  const typeFrames = Math.max(1, a.frames);
  const holdFrames = Math.max(2, Math.round(a.fps * 0.8));
  const delayMs = Math.round(1000 / a.fps);
  const frames = [];
  for (let i = 0; i <= typeFrames + holdFrames; i++) {
    const n = i <= typeFrames ? Math.round((total * i) / typeFrames) : total;
    const blink = Math.floor(i / Math.max(1, Math.round(a.fps / 2))) % 2 === 0; // ~1Hz blink
    await page.evaluate(
      ({ n, blink }) => {
        const t = document.getElementById('t');
        t.seek(n);
        const cur = t.shadowRoot.querySelector('.cursor');
        if (cur) cur.style.visibility = blink ? 'visible' : 'hidden';
      },
      { n, blink },
    );
    frames.push(await el.screenshot({ type: 'png' }));
  }
  await browser.close();

  if (a.format !== 'gif' && a.format !== 'webp') {
    console.error(`unsupported output format ".${a.format}" — use .gif or .webp`);
    process.exit(1);
  }
  const gifBuf = await encodeGifBuffer(frames, delayMs);
  if (a.format === 'gif') await writeFile(a.out, gifBuf);
  else await sharp(gifBuf, { animated: true }).webp({ quality: 90, loop: 0, effort: 4 }).toFile(a.out);
  console.log(`✓ wrote ${a.out} (${frames.length} frames, ${total} chars @ ${a.fps}fps)`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
