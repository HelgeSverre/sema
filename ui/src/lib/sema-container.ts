import { html, css } from 'lit';
import { property } from 'lit/decorators.js';
import { SemaElement } from '../internal/sema-element.js';
import { gapAttrStyles } from '../internal/layout.js';
import type { GapToken } from '../internal/layout.js';

export type ContainerSize = 'md' | 'lg' | 'full';

/**
 * `<sema-container>` — centered max-width page container with responsive
 * inline gutters. The host is the constrained box: `size` picks the max
 * inline size (`md` 1000px, `lg` 1200px — the effective default, `full`
 * unconstrained), `gutter` picks a fixed inline padding from the gap scale
 * (`none` = full bleed). When `gutter` is unset, padding is an intrinsic
 * `clamp()` — 32px on desktop easing to 24px on narrow viewports.
 *
 * Inline axis only — vertical rhythm stays in page CSS. Value precedence:
 * explicit attribute > inherited `--sema-container-max` / `--sema-container-gutter`
 * (the raw-value escape hatches) > token default; bare elements carry no
 * attributes. Semantics-neutral: no ARIA roles — consumers add landmarks.
 *
 * ```html
 * <section class="band">
 *   <sema-container>…section content…</sema-container>
 * </section>
 * <sema-container size="md">…component demos…</sema-container>
 * <nav class="site-nav">
 *   <sema-container size="full">…nav items…</sema-container>
 * </nav>
 * ```
 */
export class SemaContainer extends SemaElement {
  static styles = [
    SemaElement.base,
    css`
      :host {
        display: block;
        --_max: var(--sema-container-max, var(--container-lg, 1200px));
        --_gutter: var(--sema-container-gutter, clamp(var(--space-lg, 24px), 4vw, var(--space-xl, 32px)));
        max-inline-size: var(--_max);
        margin-inline: auto;
        padding-inline: var(--_gutter);
      }
      /* :host([size='…']) and :host([gutter='…']) overwrite --_max / --_gutter.
         These match author-set attributes only — defaults never reflect, so a
         bare element falls through to the --sema-* tier above. */
      :host([size='md']) {
        --_max: var(--container-md, 1000px);
      }
      :host([size='lg']) {
        --_max: var(--container-lg, 1200px);
      }
      :host([size='full']) {
        --_max: none;
      }
    `,
    gapAttrStyles('gutter', '--_gutter'),
  ];

  /** Max inline size: `md` 1000px / `lg` 1200px / `full` none. Unset → `lg`. */
  @property({ reflect: true }) size?: ContainerSize;
  /** Fixed inline padding from the gap scale; `none` = full bleed. Unset → responsive clamp. */
  @property({ reflect: true }) gutter?: GapToken;

  render() {
    return html`<slot></slot>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'sema-container': SemaContainer;
  }
}
customElements.define('sema-container', SemaContainer);
