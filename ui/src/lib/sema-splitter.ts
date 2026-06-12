import { html, css } from 'lit';
import { property } from 'lit/decorators.js';
import { SemaElement } from '../internal/sema-element.js';
import type { SemaResizeEventDetail } from './events.js';

export type SplitterDirection = 'horizontal' | 'vertical';

/**
 * `<sema-splitter>` — an interactive drag handle between two app-managed
 * panes. Emits `sema-resize` deltas (pointer and keyboard) for the app to
 * apply to its own layout, with `role="separator"` ARIA value state kept
 * current via `setValue()`. It owns no panes and does no layout itself.
 * For a static two-pane layout that never resizes interactively, use
 * `<sema-sidebar>`.
 */
export class SemaSplitter extends SemaElement {
  static styles = [
    SemaElement.base,
    css`
      :host {
        display: block;
        flex-shrink: 0;
        background: var(--border, #1e1e1e);
        position: relative;
        z-index: 5;
        transition: background 0.15s;
        outline: none;
      }
      :host([direction="horizontal"]) {
        width: 4px;
        cursor: col-resize;
      }
      :host([direction="vertical"]) {
        height: 4px;
        cursor: row-resize;
      }
      /* Invisible expanded hit target — a 4px bar is too thin to grab reliably. */
      :host::after {
        content: '';
        position: absolute;
      }
      :host([direction="horizontal"])::after {
        inset: 0 -4px;
      }
      :host([direction="vertical"])::after {
        inset: -4px 0;
      }
      :host(:hover),
      :host([dragging]) {
        background: var(--gold-dim, rgba(200, 168, 85, 0.5));
      }
      :host(:focus-visible) {
        outline: var(--focus-ring-width, 1px) solid var(--focus-ring-color-subtle, rgba(200, 168, 85, 0.5));
        outline-offset: var(--focus-ring-offset, 1px);
      }
    `,
  ];

  @property({ reflect: true }) direction: SplitterDirection = 'horizontal';
  @property({ type: Number }) min = 0;
  @property({ type: Number }) max = Infinity;
  @property({ type: Number }) step = 10;
  @property({ type: Number }) shiftStep = 50;

  private _startCoord = 0;
  private _endDrag: (() => void) | null = null;

  connectedCallback() {
    super.connectedCallback();
    this.setAttribute('role', 'separator');
    this.tabIndex = 0;
    if (!this.hasAttribute('aria-label')) {
      this.setAttribute('aria-label', 'Resize');
    }
    this.addEventListener('mousedown', this._onPointerDown);
    this.addEventListener('touchstart', this._onPointerDown, { passive: false });
    this.addEventListener('keydown', this._onKeydown);
  }

  disconnectedCallback() {
    super.disconnectedCallback();
    this.removeEventListener('mousedown', this._onPointerDown);
    this.removeEventListener('touchstart', this._onPointerDown);
    this.removeEventListener('keydown', this._onKeydown);
    this._endDrag?.();
  }

  updated(changed: Map<string, unknown>) {
    if (changed.has('direction')) {
      // aria-orientation describes the separator itself: a col-resize splitter
      // (direction="horizontal" layout) is a vertical bar, and vice versa.
      this.setAttribute('aria-orientation', this.direction === 'horizontal' ? 'vertical' : 'horizontal');
    }
    if (changed.has('min')) this.setAttribute('aria-valuemin', String(this.min));
    if (changed.has('max')) {
      // "Infinity" is not a valid aria-valuemax number; omit until max is finite.
      if (Number.isFinite(this.max)) this.setAttribute('aria-valuemax', String(this.max));
      else this.removeAttribute('aria-valuemax');
    }
  }

  /** Report the current pane size to assistive tech (aria-valuenow/-valuetext). */
  setValue(size: number, text?: string) {
    this.setAttribute('aria-valuenow', String(size));
    if (text !== undefined) this.setAttribute('aria-valuetext', text);
    else this.removeAttribute('aria-valuetext');
  }

  private _onPointerDown = (e: MouseEvent | TouchEvent) => {
    e.preventDefault();
    this._endDrag?.();
    const isTouch = 'touches' in e;
    const clientCoord = isTouch ? (e as TouchEvent).touches[0] : (e as MouseEvent);
    this._startCoord = this.direction === 'horizontal' ? clientCoord.clientX : clientCoord.clientY;

    this.setAttribute('dragging', '');
    document.body.style.cursor = this.direction === 'horizontal' ? 'col-resize' : 'row-resize';
    document.body.style.userSelect = 'none';

    this.dispatchEvent(new CustomEvent('sema-resize-start', { bubbles: true, composed: true }));

    const move = (ev: MouseEvent | TouchEvent) => {
      const mc = 'touches' in ev ? (ev as TouchEvent).touches[0] : (ev as MouseEvent);
      const delta = (this.direction === 'horizontal' ? mc.clientX : mc.clientY) - this._startCoord;
      this.dispatchEvent(new CustomEvent<SemaResizeEventDetail>('sema-resize', {
        detail: { delta },
        bubbles: true,
        composed: true,
      }));
    };

    const end = () => {
      this._endDrag = null;
      this.removeAttribute('dragging');
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
      document.removeEventListener('mousemove', move as EventListener);
      document.removeEventListener('mouseup', end);
      document.removeEventListener('touchmove', move as EventListener);
      document.removeEventListener('touchend', end);
      document.removeEventListener('touchcancel', end);
      window.removeEventListener('blur', end);
      // Teardown also runs from disconnectedCallback — don't dispatch from a detached node.
      if (this.isConnected) {
        this.dispatchEvent(new CustomEvent('sema-resize-end', { bubbles: true, composed: true }));
      }
    };
    this._endDrag = end;

    document.addEventListener('mousemove', move as EventListener);
    document.addEventListener('mouseup', end);
    document.addEventListener('touchmove', move as EventListener);
    document.addEventListener('touchend', end);
    document.addEventListener('touchcancel', end);
    window.addEventListener('blur', end);
  };

  private _onKeydown = (e: KeyboardEvent) => {
    const isH = this.direction === 'horizontal';
    let delta = 0;

    if (e.key === 'Home') {
      e.preventDefault();
      this.dispatchEvent(new CustomEvent('sema-resize-start', { bubbles: true, composed: true }));
      this.dispatchEvent(new CustomEvent<SemaResizeEventDetail>('sema-resize', {
        detail: { delta: this.min, absolute: true },
        bubbles: true, composed: true,
      }));
      this.dispatchEvent(new CustomEvent('sema-resize-end', { bubbles: true, composed: true }));
      return;
    }
    if (e.key === 'End') {
      e.preventDefault();
      this.dispatchEvent(new CustomEvent('sema-resize-start', { bubbles: true, composed: true }));
      this.dispatchEvent(new CustomEvent<SemaResizeEventDetail>('sema-resize', {
        detail: { delta: this.max, absolute: true },
        bubbles: true, composed: true,
      }));
      this.dispatchEvent(new CustomEvent('sema-resize-end', { bubbles: true, composed: true }));
      return;
    }

    if ((isH && e.key === 'ArrowLeft') || (!isH && e.key === 'ArrowUp')) {
      delta = e.shiftKey ? -this.shiftStep : -this.step;
    } else if ((isH && e.key === 'ArrowRight') || (!isH && e.key === 'ArrowDown')) {
      delta = e.shiftKey ? this.shiftStep : this.step;
    } else {
      return;
    }

    e.preventDefault();
    this.dispatchEvent(new CustomEvent('sema-resize-start', { bubbles: true, composed: true }));
    this.dispatchEvent(new CustomEvent<SemaResizeEventDetail>('sema-resize', {
      detail: { delta, keyboard: true },
      bubbles: true, composed: true,
    }));
    this.dispatchEvent(new CustomEvent('sema-resize-end', { bubbles: true, composed: true }));
  };

  render() {
    return html``;
  }
}

declare global { interface HTMLElementTagNameMap { 'sema-splitter': SemaSplitter } }
customElements.define('sema-splitter', SemaSplitter);
