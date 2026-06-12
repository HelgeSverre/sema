import { html, css } from 'lit';
import { property } from 'lit/decorators.js';
import { SemaElement } from '../internal/sema-element.js';

export type SpinnerSize = 'sm' | 'md' | 'lg';

/**
 * Minimal inline loading indicator. Announces via a `role="status"` live region;
 * the visual ring is hidden from assistive tech.
 *
 * @csspart spinner - the spinning ring element
 */
export class SemaSpinner extends SemaElement {
  static styles = [
    SemaElement.base,
    css`
      :host {
        display: inline-flex;
        align-items: center;
        vertical-align: middle;
        color: var(--text-secondary, #a09888);

        --_size: 16px;
        --_border: 2px;
      }
      :host([size='sm']) { --_size: 12px; --_border: 1.5px; }
      :host([size='lg']) { --_size: 24px; --_border: 2.5px; }

      .spinner {
        width: var(--_size);
        height: var(--_size);
        border: var(--_border) solid var(--border-focus, #333);
        border-top-color: var(--gold, #c8a855);
        border-radius: var(--radius-full, 50%);
        animation: spin 0.7s linear infinite;
      }

      @keyframes spin {
        to { transform: rotate(360deg); }
      }

      .visually-hidden {
        position: absolute;
        width: 1px;
        height: 1px;
        padding: 0;
        margin: -1px;
        overflow: hidden;
        clip: rect(0, 0, 0, 0);
        white-space: nowrap;
        border: 0;
      }
    `,
  ];

  @property({ reflect: true }) size: SpinnerSize = 'md';
  /** Accessible label announced by the status live region. */
  @property() label = 'Loading';

  render() {
    return html`
      <span class="spinner" part="spinner" aria-hidden="true"></span>
      <span class="visually-hidden" role="status">${this.label}</span>
    `;
  }
}

declare global { interface HTMLElementTagNameMap { 'sema-spinner': SemaSpinner } }
customElements.define('sema-spinner', SemaSpinner);
