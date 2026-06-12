import { html, css } from 'lit';
import { property } from 'lit/decorators.js';
import { SemaElement } from '../internal/sema-element.js';
import { bridgeLengthAttr, gapAttrStyles } from '../internal/layout.js';
import type { GapToken } from '../internal/layout.js';

export type GridCols = 2;

/**
 * `<sema-grid>` — responsive grid for cards/swatches/icons, in two modes.
 * **Auto mode** (`min`, default): `auto-fill` columns at least `min` wide
 * (effective default 240px), clamped to `100%` so the grid never overflows a
 * narrow parent. **Fixed mode** (`cols="2"`): two equal columns collapsing to
 * one below a 700px *container* width — it responds to its own width, not the
 * viewport, so it nests correctly. Setting `cols` ignores `min`.
 *
 * Items are light-DOM children and grid items of the shadow grid, so per-item
 * spans are plain page CSS — `my-card { grid-column: span 2 }` — no API
 * needed (tracks never align *across* grid instances). Because an unset `min`
 * falls through to the inherited `--sema-grid-min` custom property, page CSS
 * can re-tune the column minimum responsively:
 *
 * ```css
 * @media (max-width: 768px) {
 *   .swatch-grid-host { --sema-grid-min: 180px; }
 * }
 * ```
 *
 * Height caveat: `height`/`min-height` on the host never reaches the tracks —
 * they live in the inner wrapper; use `sema-grid::part(grid) { min-block-size: 70vh }`.
 * Semantics-neutral: no ARIA roles — consumers add `role="list"` etc.
 *
 * ```html
 * <sema-grid min="280px">
 *   <div class="card-feature">…</div>
 *   <div class="card-feature">…</div>
 * </sema-grid>
 * <sema-grid cols="2" gap="xl">
 *   <div>pane A</div>
 *   <div>pane B</div>
 * </sema-grid>
 * ```
 */
export class SemaGrid extends SemaElement {
  static styles = [
    SemaElement.base,
    css`
      :host {
        display: block;
        --_min: var(--sema-grid-min, 240px);
        --_gap: var(--sema-grid-gap, var(--space-md, 16px));
      }
      /* Named inline-size container, and only when cols is set: auto mode
         needs no containment, and the name prevents accidental matches
         against consumer-defined ancestor containers. */
      :host([cols]) {
        container: sema-grid / inline-size;
      }
      /* Inner wrapper because container queries can't style the container's
         own box — shadow content queries the host. */
      .grid {
        display: grid;
        gap: var(--_gap);
        grid-template-columns: repeat(auto-fill, minmax(min(var(--_min), 100%), 1fr));
      }
      :host([cols='2']) .grid {
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      /* The collapse must beat :host([cols='2']) above regardless of source
         order: the doubled [cols][cols] bumps specificity so a refactor (or
         codegen emitting blocks in a different order) can't silently break
         the narrow-container collapse. Threshold 700px ≈ content width of the
         default container at the page's historic 768px viewport breakpoint
         (768 − 2×32px gutters), so top-level grids collapse at the same point
         they did before migration. */
      @container sema-grid (inline-size < 700px) {
        :host([cols][cols]) .grid {
          grid-template-columns: minmax(0, 1fr);
        }
      }
      /* Code blocks / long URLs can't blow out tracks. */
      ::slotted(*) {
        min-inline-size: 0;
      }
    `,
    gapAttrStyles('gap', '--_gap'),
  ];

  /** Auto mode minimum column width (any CSS length). Unset → 240px. */
  @property({ reflect: true }) min?: string;
  /** Fixed mode: 2 equal columns, collapsing to 1 below a 700px container width. */
  @property({ type: Number, reflect: true }) cols?: GridCols;
  /** Grid gap from the gap scale. Unset → `md` (16px). */
  @property({ reflect: true }) gap?: GapToken;

  willUpdate(changed: Map<string, unknown>) {
    if (changed.has('min')) bridgeLengthAttr(this, this.min, '--_min');
  }

  render() {
    return html`<div class="grid" part="grid"><slot></slot></div>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'sema-grid': SemaGrid;
  }
}
customElements.define('sema-grid', SemaGrid);
