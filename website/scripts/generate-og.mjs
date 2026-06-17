// Generates per-page OpenGraph card images from website/og-template.html.
//
// Renders the template in headless Chromium (via Playwright) at 1200x630,
// once per page, driving its homepage/docs variants through URL query params.
// Output: website/public/og/<slug>.png  (slug from og.shared.mjs::ogSlug)
//
// Usage:
//   node scripts/generate-og.mjs            # all pages
//   node scripts/generate-og.mjs home docs  # only matching slugs (substring)
//
// Re-run after editing og-template.html, the logo, page titles, or the
// version, then commit the regenerated public/og/*.png before deploying.

import { chromium } from 'playwright'
import { readFileSync, readdirSync, mkdirSync, statSync } from 'node:fs'
import { fileURLToPath, pathToFileURL } from 'node:url'
import { dirname, join, relative } from 'node:path'
import { OG_WIDTH, OG_HEIGHT, OG_EXT, ogSlug, categoryFor, isHomepage } from '../.vitepress/og.shared.mjs'

const __dirname = dirname(fileURLToPath(import.meta.url))
const WEBSITE = join(__dirname, '..')
const TEMPLATE = join(WEBSITE, 'og-template.html')
const OUT_DIR = join(WEBSITE, 'public', 'og')
const filters = process.argv.slice(2)

// Current Sema version, shown as the badge on docs cards.
function semaVersion() {
  try {
    const cargo = readFileSync(join(WEBSITE, '..', 'Cargo.toml'), 'utf8')
    const m = cargo.match(/^\s*version\s*=\s*"([^"]+)"/m)
    return m ? `v${m[1]}` : 'docs'
  } catch {
    return 'docs'
  }
}

// Recursively collect every .md page under website/ (excluding node_modules).
function collectPages(dir) {
  const out = []
  for (const name of readdirSync(dir)) {
    if (name === 'node_modules' || name === '.vitepress' || name === 'public') continue
    const full = join(dir, name)
    const st = statSync(full)
    if (st.isDirectory()) out.push(...collectPages(full))
    else if (name.endsWith('.md')) out.push(full)
  }
  return out
}

function stripInline(s) {
  return s
    .replace(/`([^`]+)`/g, '$1') // inline code
    .replace(/\[([^\]]+)\]\([^)]*\)/g, '$1') // links -> text
    .replace(/[*_~]/g, '') // emphasis
    .replace(/\s+/g, ' ')
    .trim()
}

// Pull a title (first H1) and description (frontmatter `description:` or first
// real paragraph) out of a markdown file.
function pageMeta(md) {
  let body = md
  let fmDescription = null
  const fm = md.match(/^---\n([\s\S]*?)\n---\n?/)
  if (fm) {
    const d = fm[1].match(/^description:\s*(.+)$/m)
    if (d) fmDescription = stripInline(d[1].replace(/^["']|["']$/g, ''))
    body = md.slice(fm[0].length)
  }
  const lines = body.split('\n')
  let title = null
  let descParts = []
  let seenH1 = false
  for (const line of lines) {
    const h1 = line.match(/^#\s+(.+)$/)
    if (!seenH1) {
      if (h1) {
        title = stripInline(h1[1])
        seenH1 = true
      }
      continue
    }
    const t = line.trim()
    if (!t) {
      if (descParts.length) break
      continue
    }
    if (/^[#>`|:-]|^!\[|^<|^\s*[-*]\s/.test(t)) {
      if (descParts.length) break
      continue
    }
    descParts.push(t)
    if (descParts.join(' ').length > 200) break
  }
  let description = fmDescription || stripInline(descParts.join(' '))
  if (description.length > 165) {
    let cut = description.slice(0, 162)
    const lastSpace = cut.lastIndexOf(' ')
    if (lastSpace > 120) cut = cut.slice(0, lastSpace) // break on a word boundary
    description = cut.replace(/[\s.,;:—-]+$/, '') + '…'
  }
  return { title, description }
}

const BADGE = semaVersion()

function paramsForPage(relPath, md) {
  if (isHomepage(relPath)) return { variant: 'homepage' } // keep designed defaults
  const { title, description } = pageMeta(md)
  return {
    variant: 'docs',
    title: title || 'Sema',
    subtitle: description || 'A Lisp with first-class LLM primitives, implemented in Rust.',
    category: categoryFor(relPath),
    badge: BADGE,
  }
}

function templateUrl(params) {
  const u = pathToFileURL(TEMPLATE)
  for (const [k, v] of Object.entries(params)) u.searchParams.set(k, v)
  return u.toString()
}

const pages = collectPages(WEBSITE).map((full) => {
  const relPath = relative(WEBSITE, full).replace(/\\/g, '/')
  return { full, relPath, slug: ogSlug(relPath), out: join(OUT_DIR, `${ogSlug(relPath)}.${OG_EXT}`) }
})

// One-off cards that live outside the website docs tree (different product /
// domain / output path) but share the same template + brand.
const SPECIAL = [
  {
    slug: 'playground',
    out: join(WEBSITE, '..', 'playground', 'og-playground.jpg'),
    params: {
      variant: 'homepage',
      titleHtml: 'Sema <span>Playground</span>',
      subtitle:
        'Try Sema in your browser — a Lisp with first-class LLM primitives. No install, runs entirely in WebAssembly.',
      domain: 'sema.run',
    },
  },
]

mkdirSync(OUT_DIR, { recursive: true })

const jobs = [
  ...pages.map((p) => ({
    slug: p.slug,
    out: p.out,
    params: paramsForPage(p.relPath, readFileSync(p.full, 'utf8')),
    match: `${p.slug} ${p.relPath}`,
  })),
  ...SPECIAL.map((s) => ({ slug: s.slug, out: s.out, params: s.params, match: s.slug })),
]

const selected = filters.length ? jobs.filter((j) => filters.some((f) => j.match.includes(f))) : jobs

console.log(`Generating ${selected.length} OG image(s)  [badge ${BADGE}]`)

const browser = await chromium.launch()
const page = await browser.newPage({
  viewport: { width: OG_WIDTH, height: OG_HEIGHT },
  deviceScaleFactor: 1,
})

let ok = 0
for (const j of selected) {
  await page.goto(templateUrl(j.params), { waitUntil: 'load' })
  await page.waitForSelector('html[data-og-ready="1"]', { timeout: 10000 }).catch(() => {})
  mkdirSync(dirname(j.out), { recursive: true })
  await page.screenshot({
    path: j.out,
    type: 'jpeg',
    quality: 92,
    clip: { x: 0, y: 0, width: OG_WIDTH, height: OG_HEIGHT },
  })
  ok++
  const label = j.params.title || j.params.titleHtml?.replace(/<[^>]+>/g, '') || ''
  console.log(`  ✓ ${relative(join(WEBSITE, '..'), j.out)}  (${j.params.variant}${label ? ` — ${label}` : ''})`)
}

await browser.close()
console.log(`Done: ${ok}/${selected.length} images written.`)
