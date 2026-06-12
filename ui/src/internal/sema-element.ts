import { LitElement, css } from 'lit';

export class SemaElement extends LitElement {
  static base = css`
    :host {
      box-sizing: border-box;
    }
    :host *,
    :host *::before,
    :host *::after {
      box-sizing: border-box;
    }
    @media (prefers-reduced-motion: reduce) {
      /* Near-zero (not zero) so animationend/transitionend still fire. */
      :host,
      :host *,
      :host *::before,
      :host *::after {
        animation-duration: 0.001ms !important;
        transition-duration: 0.001ms !important;
        animation-iteration-count: 1 !important;
        scroll-behavior: auto;
      }
    }
  `;
}
