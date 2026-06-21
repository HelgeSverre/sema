/**
 * Sema-aware syntax highlighting for <sema-code>.
 *
 * Strategy: use Shiki purely as a TextMate *tokenizer* — it loads grammars and
 * produces per-token scope names. We then map each scope to a flat `tok-*` class
 * (see syntax.css) and emit our own spans, so the output is themeable via the
 * --syntax-* CSS variables instead of Shiki's inline colors. Because the scope→class
 * map keys on standard TextMate scope prefixes (`string`, `constant.numeric`, …),
 * most languages colorize correctly with the shared palette.
 *
 * `sema` is bundled (vendored grammar). Other languages are lazy-loaded as separate
 * chunks on first use (see LANG_LOADERS). Consumers can add languages or custom
 * grammars via `registerLanguage()`.
 *
 * Grammars are plain TextMate regex, so the lightweight JavaScript RegExp engine is
 * used — no Oniguruma WASM is bundled.
 */
import {
  createHighlighterCore,
  type HighlighterCore,
  type LanguageRegistration,
} from 'shiki/core';
import { createJavaScriptRegexEngine } from 'shiki/engine/javascript';
import semaGrammar from '../grammars/sema.tmLanguage.json';

// The vendored grammar declares scopeName "source.sema"; give it the Shiki id "sema".
const SEMA_LANG = { ...semaGrammar, name: 'sema' } as unknown as LanguageRegistration;

/**
 * Built-in languages, lazy-loaded as separate chunks. Each loader is an explicit
 * static `import()` (NOT a template-literal specifier) so bundlers code-split per
 * language — consumers only download the grammars they actually highlight.
 */
const LANG_LOADERS: Record<string, () => Promise<unknown>> = {
  json: () => import('@shikijs/langs/json'),
  shellscript: () => import('@shikijs/langs/shellscript'),
  javascript: () => import('@shikijs/langs/javascript'),
  typescript: () => import('@shikijs/langs/typescript'),
  html: () => import('@shikijs/langs/html'),
  css: () => import('@shikijs/langs/css'),
  toml: () => import('@shikijs/langs/toml'),
  markdown: () => import('@shikijs/langs/markdown'),
  rust: () => import('@shikijs/langs/rust'),
  yaml: () => import('@shikijs/langs/yaml'),
  diff: () => import('@shikijs/langs/diff'),
};

/** Common aliases → canonical loader id. */
const ALIASES: Record<string, string> = {
  sh: 'shellscript',
  bash: 'shellscript',
  shell: 'shellscript',
  zsh: 'shellscript',
  js: 'javascript',
  ts: 'typescript',
  md: 'markdown',
  rs: 'rust',
  yml: 'yaml',
};

/** Consumer-registered custom grammars (id → registration) and extra lazy loaders. */
const customGrammars = new Map<string, LanguageRegistration>();
const customLoaders = new Map<string, () => Promise<unknown>>();

function resolveLang(lang: string): string {
  return ALIASES[lang] ?? lang;
}

/** Whether `lang` can be highlighted (built-in, alias, or registered). */
export function canHighlight(lang: string): boolean {
  const id = resolveLang(lang);
  return (
    id === 'sema' ||
    id in LANG_LOADERS ||
    customGrammars.has(id) ||
    customLoaders.has(id)
  );
}

/**
 * Register a language so `<sema-code lang="…">` can highlight it.
 *
 * - `registerLanguage(grammar)` — a ready TextMate grammar object (uses its `name`).
 * - `registerLanguage(id, loader)` — a lazy loader, e.g. `() => import('@shikijs/langs/php')`.
 *
 * The canonical, global entry point. `SemaCode.registerLanguage` is an alias of this.
 */
export function registerLanguage(grammar: LanguageRegistration): void;
export function registerLanguage(id: string, loader: () => Promise<unknown>): void;
export function registerLanguage(
  a: LanguageRegistration | string,
  loader?: () => Promise<unknown>,
): void {
  if (typeof a === 'string') {
    if (loader) customLoaders.set(a, loader);
  } else {
    customGrammars.set(a.name, a);
  }
}

/** TextMate scope (most specific first) -> tok-* class. First prefix match wins. */
const SCOPE_TO_CLASS: ReadonlyArray<readonly [string, string]> = [
  ['constant.character.escape', 'tok-escape'],
  ['constant.numeric', 'tok-number'],
  ['constant.language', 'tok-boolean'],
  ['constant.character', 'tok-boolean'],
  ['constant.other.keyword', 'tok-keyword-lit'],
  ['support.type.property-name', 'tok-property'], // JSON object keys
  ['meta.object-literal.key', 'tok-property'], // JS/TS object keys
  ['entity.name.tag', 'tok-keyword'], // HTML/XML tags
  ['entity.other.attribute-name', 'tok-property'], // HTML/XML attributes
  ['comment', 'tok-comment'],
  ['string', 'tok-string'],
  ['keyword.operator', 'tok-operator'],
  ['keyword', 'tok-keyword'],
  ['entity.name.function', 'tok-function'],
  ['support.function', 'tok-builtin'],
  ['variable', 'tok-variable'],
  ['punctuation.definition.comment', 'tok-comment'], // the `#`/`//` delimiter is part of the comment
  ['punctuation', 'tok-punctuation'],
];

function classForScope(scope: string): string | null {
  for (const [prefix, cls] of SCOPE_TO_CLASS) {
    if (scope === prefix || scope.startsWith(prefix + '.')) return cls;
  }
  return null;
}

export function escapeHtml(s: string): string {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

// Minimal theme — its colors are discarded (we map scopes to classes ourselves),
// it only exists because Shiki's tokenizer requires a registered theme.
const NOOP_THEME = {
  name: 'sema-noop',
  settings: [{ settings: { foreground: '#d8d0c0', background: '#0a0a0a' } }],
};

let highlighterPromise: Promise<HighlighterCore> | null = null;

function getHighlighter(): Promise<HighlighterCore> {
  if (!highlighterPromise) {
    highlighterPromise = createHighlighterCore({
      themes: [NOOP_THEME],
      langs: [SEMA_LANG],
      engine: createJavaScriptRegexEngine({ forgiving: true }),
    });
  }
  return highlighterPromise;
}

const loaded = new Set<string>(['sema']);
const loading = new Map<string, Promise<void>>();

/** Ensure a language's grammar is loaded into the highlighter (deduped, cached). */
async function ensureLanguage(id: string): Promise<void> {
  if (loaded.has(id)) return;
  const existing = loading.get(id);
  if (existing) return existing;

  const p = (async () => {
    const hl = await getHighlighter();
    if (customGrammars.has(id)) {
      await hl.loadLanguage(customGrammars.get(id)!);
    } else {
      const load = customLoaders.get(id) ?? LANG_LOADERS[id];
      if (!load) return;
      const mod = await load();
      // Bundled grammars export a default array; custom loaders may return either.
      const langs = (mod && typeof mod === 'object' && 'default' in mod
        ? (mod as { default: unknown }).default
        : mod) as LanguageRegistration | LanguageRegistration[];
      await hl.loadLanguage(langs);
    }
    loaded.add(id);
  })();

  loading.set(id, p);
  try {
    await p;
  } finally {
    loading.delete(id);
  }
}

/**
 * Highlight `code` and return the inner HTML for a `<pre><code>` element:
 * per-token `<span class="tok-*">` (HTML-escaped), lines joined by "\n".
 *
 * For an unsupported `lang` (or if grammar load/tokenize fails), returns the escaped
 * plain text (no spans) so callers can render it verbatim.
 */
export async function highlightToHtml(code: string, lang: string): Promise<string> {
  const id = resolveLang(lang);
  if (!canHighlight(id)) return escapeHtml(code);

  await ensureLanguage(id);
  if (!loaded.has(id)) return escapeHtml(code);

  const hl = await getHighlighter();
  let lines;
  try {
    lines = hl.codeToTokensBase(code, {
      lang: id,
      theme: 'sema-noop',
      includeExplanation: true,
    });
  } catch {
    return escapeHtml(code);
  }

  return lines
    .map((line) =>
      line
        .map((token) => {
          // Each token may cover several scoped sub-segments; render per segment.
          const parts = token.explanation ?? [{ content: token.content, scopes: [] }];
          return parts
            .map((part) => {
              const scope = part.scopes[part.scopes.length - 1]?.scopeName ?? '';
              const cls = classForScope(scope);
              const text = escapeHtml(part.content);
              return cls ? `<span class="${cls}">${text}</span>` : text;
            })
            .join('');
        })
        .join(''),
    )
    .join('\n');
}

/** A highlighted segment: raw text + its `tok-*` class (empty for unscoped text). */
export interface CodeToken {
  text: string;
  cls: string;
}

/**
 * Tokenize `code` into a flat list of `{ text, cls }` segments (newlines preserved as
 * their own `{ text: "\n", cls: "" }` tokens). Same scope→class mapping as
 * `highlightToHtml`, but returns data instead of HTML — for consumers that reveal code
 * progressively (e.g. `<sema-code-typer>`). Falls back to a single unscoped token on an
 * unsupported lang or tokenize failure. The concatenated `text` equals `code` exactly.
 */
export async function tokenize(code: string, lang: string): Promise<CodeToken[]> {
  const fallback = (): CodeToken[] => (code ? [{ text: code, cls: '' }] : []);
  const id = resolveLang(lang);
  if (!canHighlight(id)) return fallback();

  await ensureLanguage(id);
  if (!loaded.has(id)) return fallback();

  const hl = await getHighlighter();
  let lines;
  try {
    lines = hl.codeToTokensBase(code, { lang: id, theme: 'sema-noop', includeExplanation: true });
  } catch {
    return fallback();
  }

  const out: CodeToken[] = [];
  lines.forEach((line, i) => {
    if (i > 0) out.push({ text: '\n', cls: '' });
    for (const token of line) {
      const parts = token.explanation ?? [{ content: token.content, scopes: [] }];
      for (const part of parts) {
        if (!part.content) continue;
        const scope = part.scopes[part.scopes.length - 1]?.scopeName ?? '';
        out.push({ text: part.content, cls: classForScope(scope) ?? '' });
      }
    }
  });
  return out;
}
