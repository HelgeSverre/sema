import { html, css } from 'lit';
import { property, query } from 'lit/decorators.js';
import { SemaElement } from '../internal/sema-element.js';
import { FocusTrapController, getFocusableElements } from '../internal/controllers/focus-trap.js';

export type PopoverPlacement =
  | 'bottom-start'
  | 'bottom-end'
  | 'top-start'
  | 'top-end'
  | 'left'
  | 'right';

/**
 * `<sema-popover>` — a generic anchored popover.
 *
 * Put the trigger in `slot="trigger"` and the floating content in the default slot.
 * Opens on trigger click (or hover via `open-on="hover"`).
 *
 * **Focus & keyboard (WAI-ARIA menu-button pattern):** opening moves focus into the
 * panel (a child `<sema-menu>` focuses its first item); Escape, outside-click, Tab, and
 * focus leaving the popover all close it; Escape/Tab return focus to the trigger. The
 * trigger gets `aria-haspopup` + `aria-expanded`. Add the **`modal`** attribute for
 * dialog-like content that should hard-trap Tab instead of closing on Tab/focus-out.
 *
 * Emits `sema-open` / `sema-close`. A child `<sema-menu>`'s `sema-select` auto-closes it.
 */
export class SemaPopover extends SemaElement {
  static styles = [
    SemaElement.base,
    css`
      :host {
        display: inline-block;
        position: relative;
      }
      .panel {
        position: absolute;
        z-index: 300;
        min-width: max-content;
        background: var(--bg-elevated, #141414);
        border: 1px solid var(--border, #1e1e1e);
        border-radius: var(--radius-md, 4px);
        padding: var(--space-xs, 4px);
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.4);
      }
      .panel[hidden] {
        display: none;
      }
      :host([placement='bottom-start']) .panel {
        top: calc(100% + 4px);
        left: 0;
      }
      :host([placement='bottom-end']) .panel {
        top: calc(100% + 4px);
        right: 0;
      }
      :host([placement='top-start']) .panel {
        bottom: calc(100% + 4px);
        left: 0;
      }
      :host([placement='top-end']) .panel {
        bottom: calc(100% + 4px);
        right: 0;
      }
      :host([placement='left']) .panel {
        right: calc(100% + 4px);
        top: 0;
      }
      :host([placement='right']) .panel {
        left: calc(100% + 4px);
        top: 0;
      }
    `,
  ];

  /** Open state. */
  @property({ type: Boolean, reflect: true }) open = false;
  /** Where the panel sits relative to the trigger. */
  @property({ reflect: true }) placement: PopoverPlacement = 'bottom-start';
  /** How the popover opens. */
  @property({ attribute: 'open-on' }) openOn: 'click' | 'hover' = 'click';
  /** Hard-trap Tab focus inside the panel (for dialog-like content). */
  @property({ type: Boolean, reflect: true }) modal = false;

  @query('.panel') private _panel!: HTMLElement;
  private _triggerEl: HTMLElement | null = null;

  // Trap focus only for modal popovers; menus self-manage focus (roving tabindex).
  private _focusTrap = new FocusTrapController(this, {
    getContainer: () => this._panel,
    isActive: () => this.open && this.modal,
  });

  private _onDocPointer = (e: Event) => {
    if (!e.composedPath().includes(this)) this.hide(false);
  };

  disconnectedCallback() {
    super.disconnectedCallback();
    document.removeEventListener('pointerdown', this._onDocPointer, true);
    // Don't leave a reconnected popover stuck-open with a stale aria-expanded.
    if (this.open) {
      this._triggerEl?.setAttribute('aria-expanded', 'false');
      this.open = false;
    }
    this._triggerEl = null;
  }

  private get _trigger(): HTMLElement | null {
    return this.querySelector<HTMLElement>('[slot="trigger"]');
  }

  show() {
    if (this.open) return;
    this._triggerEl = this._trigger;
    this.open = true;
    const trigger = this._triggerEl;
    if (trigger) {
      trigger.setAttribute('aria-expanded', 'true');
      trigger.setAttribute(
        'aria-haspopup',
        this.querySelector('sema-menu') ? 'menu' : this.modal ? 'dialog' : 'true',
      );
    }
    document.addEventListener('pointerdown', this._onDocPointer, true);
    this.dispatchEvent(new CustomEvent('sema-open', { bubbles: true, composed: true }));
    // Move focus into the panel (a menu focuses its first item).
    this.updateComplete.then(() => {
      if (!this.open) return;
      const menu = this.querySelector('sema-menu') as (HTMLElement & { focusFirst?: () => void }) | null;
      if (menu?.focusFirst) menu.focusFirst();
      // getFocusableElements walks shadow roots + slots, so it finds slotted content
      // (a plain panel.querySelector cannot cross the slot/shadow boundary).
      else getFocusableElements(this._panel)[0]?.focus();
    });
  }

  /** Close the popover. By default returns focus to the trigger (Esc/Tab/select). */
  hide(restoreFocus = true) {
    if (!this.open) return;
    this.open = false;
    document.removeEventListener('pointerdown', this._onDocPointer, true);
    // Resolve the live trigger (it may have been re-slotted) and fall back to the cached ref.
    const trigger = this._trigger ?? this._triggerEl;
    trigger?.setAttribute('aria-expanded', 'false');
    if (restoreFocus) trigger?.focus({ preventScroll: true });
    this.dispatchEvent(new CustomEvent('sema-close', { bubbles: true, composed: true }));
  }

  toggle() {
    if (this.open) this.hide();
    else this.show();
  }

  private _onTriggerClick = () => {
    if (this.openOn === 'click') this.toggle();
  };
  private _onPointerEnter = () => {
    if (this.openOn === 'hover') this.show();
  };
  private _onPointerLeave = () => {
    if (this.openOn === 'hover') this.hide(false);
  };

  private _onKeydown = (e: KeyboardEvent) => {
    if (!this.open) return;
    if (e.key === 'Escape') {
      e.stopPropagation();
      e.preventDefault();
      this.hide(true);
    } else if (e.key === 'Tab' && !this.modal) {
      // Menus don't trap: close and let Tab move focus on from the trigger.
      this.hide(true);
    }
  };

  private _onFocusOut = (e: FocusEvent) => {
    if (this.modal || !this.open) return;
    const next = e.relatedTarget as Node | null;
    // `next` retargets to the light-DOM ancestor when crossing shadow boundaries,
    // so light-DOM containment is the right check for slotted menus/content.
    if (next && this.contains(next)) return;
    this.hide(false);
  };

  private _onSelect = () => this.hide(true); // child menu chose an item

  render() {
    return html`
      <span
        class="trigger"
        part="trigger"
        @click=${this._onTriggerClick}
        @pointerenter=${this._onPointerEnter}
        @pointerleave=${this._onPointerLeave}
        @keydown=${this._onKeydown}
      >
        <slot name="trigger"></slot>
      </span>
      <div
        class="panel"
        part="panel"
        role="presentation"
        ?hidden=${!this.open}
        @keydown=${this._onKeydown}
        @focusout=${this._onFocusOut}
        @sema-select=${this._onSelect}
        @pointerenter=${this._onPointerEnter}
        @pointerleave=${this._onPointerLeave}
      >
        <slot></slot>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'sema-popover': SemaPopover;
  }
}
customElements.define('sema-popover', SemaPopover);
