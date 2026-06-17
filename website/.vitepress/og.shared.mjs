// Shared helpers for the OpenGraph image pipeline.
// Imported by both the generator (scripts/generate-og.mjs) and the VitePress
// config (config.ts) so the slug/category logic can never drift between the
// images that get written and the <meta> tags that reference them.

export const SITE = 'https://sema-lang.com'
export const OG_WIDTH = 1200
export const OG_HEIGHT = 630
// JPEG keeps the gradient/dotted-grid cards ~5x smaller than PNG with no
// visible loss at social-card display sizes.
export const OG_EXT = 'jpg'

/**
 * Map a VitePress `relativePath` (e.g. "docs/stdlib/lists.md") to a stable
 * slug used as the OG image filename: public/og/<slug>.png.
 *   index.md            -> "home"
 *   docs/index.md       -> "docs"
 *   docs/stdlib/index.md-> "docs-stdlib"
 *   docs/stdlib/lists.md-> "docs-stdlib-lists"
 */
export function ogSlug(relativePath) {
  let p = relativePath.replace(/\\/g, '/').replace(/\.md$/, '')
  if (p === 'index') return 'home'
  p = p.replace(/\/index$/, '')
  return p.replace(/\//g, '-')
}

/** Section label shown on docs cards, derived from the path prefix. */
export function categoryFor(relativePath) {
  const p = relativePath.replace(/\\/g, '/')
  if (p.startsWith('docs/stdlib/')) return 'Standard Library'
  if (p.startsWith('docs/llm/')) return 'LLM Primitives'
  if (p.startsWith('docs/internals/')) return 'Internals'
  if (p.startsWith('docs/language/')) return 'Language Reference'
  if (p.startsWith('docs/tutorial/')) return 'Tutorial'
  return 'Documentation'
}

/** True for the site landing page, which uses the homepage card design. */
export function isHomepage(relativePath) {
  const p = relativePath.replace(/\\/g, '/')
  return p === 'index.md'
}
