import { html, css, unsafeCSS } from 'lit';
import { property } from 'lit/decorators.js';
import { ifDefined } from 'lit/directives/if-defined.js';
import { live } from 'lit/directives/live.js';
import { SemaElement } from '../internal/sema-element.js';
import controlStyles from '../styles/control.css?inline';
import scrollbarStyles from '../styles/scrollbar.css?inline';

const SUPPORTS_FIELD_SIZING =
  typeof CSS !== 'undefined' && typeof CSS.supports === 'function' && CSS.supports('field-sizing', 'content');

/**
 * `<sema-textarea>` — a themed, form-associated multi-line text input.
 *
 * Uses the shared themed scrollbar. Add **`autosize`** to grow with content: via CSS
 * `field-sizing: content` where supported, with a `scrollHeight` JS fallback otherwise.
 */
export class SemaTextarea extends SemaElement {
  static formAssociated = true;
  static styles = [
    SemaElement.base,
    unsafeCSS(controlStyles),
    unsafeCSS(scrollbarStyles),
    css`
      :host {
        display: block;
      }
      .control {
        resize: vertical;
        min-height: 4em;
      }
      :host([autosize]) .control {
        field-sizing: content;
        resize: none;
        overflow: hidden;
        min-height: 3lh;
        max-height: 16lh;
      }
    `,
  ];

  @property() value = '';
  @property() placeholder = '';
  @property() name = '';
  @property({ type: Number }) rows = 4;
  @property({ type: Boolean, reflect: true }) disabled = false;
  @property({ type: Boolean, reflect: true }) required = false;
  /** Grow to fit content (CSS field-sizing, with a scrollHeight JS fallback). */
  @property({ type: Boolean, reflect: true }) autosize = false;

  private _internals = this.attachInternals();

  private get _ta(): HTMLTextAreaElement | null {
    return this.shadowRoot?.querySelector('textarea') ?? null;
  }

  // Host aria-* attributes (set e.g. by <sema-field>) must be mirrored onto the
  // inner control, where AT computes name/description — re-render when they change.
  static get observedAttributes() {
    return [...super.observedAttributes, 'aria-label', 'aria-description', 'aria-invalid'];
  }

  attributeChangedCallback(name: string, old: string | null, value: string | null) {
    super.attributeChangedCallback(name, old, value);
    if (name.startsWith('aria-')) this.requestUpdate();
  }

  updated(changed: Map<string, unknown>) {
    if (changed.has('value')) this._internals.setFormValue(this.value);
    if (this.autosize && !SUPPORTS_FIELD_SIZING) this._autoGrow();
  }

  formResetCallback() {
    this.value = '';
    this._internals.setFormValue('');
  }

  /** scrollHeight fallback for browsers without CSS `field-sizing`. */
  private _autoGrow() {
    const ta = this._ta;
    if (!ta) return;
    ta.style.height = 'auto';
    ta.style.height = `${ta.scrollHeight + 2}px`;
  }

  private _onInput = (e: Event) => {
    this.value = (e.target as HTMLTextAreaElement).value;
    this._internals.setFormValue(this.value);
    if (this.autosize && !SUPPORTS_FIELD_SIZING) this._autoGrow();
  };
  private _onChange = () => {
    this.dispatchEvent(new Event('change', { bubbles: true, composed: true }));
  };

  render() {
    return html`<textarea
      class="control sema-scroll"
      part="control"
      .value=${live(this.value)}
      rows=${this.rows}
      placeholder=${this.placeholder}
      ?disabled=${this.disabled}
      ?required=${this.required}
      aria-label=${this.getAttribute('aria-label') || this.name || 'textarea'}
      aria-description=${ifDefined(this.getAttribute('aria-description') ?? undefined)}
      aria-invalid=${ifDefined(this.getAttribute('aria-invalid') ?? undefined)}
      @input=${this._onInput}
      @change=${this._onChange}
    ></textarea>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'sema-textarea': SemaTextarea;
  }
}
customElements.define('sema-textarea', SemaTextarea);
