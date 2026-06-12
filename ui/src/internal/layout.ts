import { unsafeCSS } from 'lit';
import type { CSSResult } from 'lit';

// Shared helpers for the layout primitives (sema-container, sema-grid,
// sema-sidebar). Private module — only the GapToken type is public, re-exported
// through the barrels.

export const GAP_TOKENS = ['none', 'xs', 'sm', 'md', 'lg', 'xl', '2xl', '3xl', '4xl'] as const;
export type GapToken = (typeof GAP_TOKENS)[number];

// Pixel fallbacks mirror the --space-* scale in styles/tokens.css.
const SPACE_FALLBACK: Record<Exclude<GapToken, 'none'>, string> = {
  xs: '4px',
  sm: '8px',
  md: '16px',
  lg: '24px',
  xl: '32px',
  '2xl': '48px',
  '3xl': '64px',
  '4xl': '96px',
};

/**
 * The nine `:host([<attr>='<token>'])` rules mapping the gap enum onto a
 * private custom property. These only ever match author-set attributes —
 * defaults never reflect, so unset instances fall through to the inherited
 * `--sema-*` tier in the host's `var()` fallback chain.
 */
export function gapAttrStyles(attr: string, privateVar: string): CSSResult {
  return unsafeCSS(
    GAP_TOKENS.map((token) => {
      const value = token === 'none' ? '0' : `var(--space-${token}, ${SPACE_FALLBACK[token]})`;
      return `:host([${attr}='${token}']) { ${privateVar}: ${value}; }`;
    }).join('\n'),
  );
}

/**
 * Attribute → custom-property bridge for arbitrary-length attributes. Writes
 * the inline private property only when the value is set, and removes it when
 * the value is `undefined`/`null` (the default state), keeping the `--sema-*`
 * tier live on un-set instances. Call from `willUpdate`.
 */
export function bridgeLengthAttr(el: HTMLElement, value: string | null | undefined, privateVar: string): void {
  if (value === undefined || value === null) el.style.removeProperty(privateVar);
  else el.style.setProperty(privateVar, value);
}
