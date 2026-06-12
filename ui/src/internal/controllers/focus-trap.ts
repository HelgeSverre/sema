import type { ReactiveController, ReactiveControllerHost } from 'lit';

const FOCUSABLE =
  'a[href],button:not([disabled]),textarea:not([disabled]),input:not([disabled]),select:not([disabled]),[tabindex]:not([tabindex="-1"])';

interface FocusTrapOptions<Host extends ReactiveControllerHost & HTMLElement> {
  getContainer: (host: Host) => HTMLElement | null | undefined;
  isActive: (host: Host) => boolean;
  lockScroll?: boolean;
  initialFocus?: 'first-focusable' | 'autofocus';
}

export function getFocusableElements(
  root: Element,
): HTMLElement[] {
  const results: HTMLElement[] = [];

  function walk(el: Element) {
    if (el instanceof HTMLElement && el.matches(FOCUSABLE) && !results.includes(el)) {
      results.push(el);
    }

    if (el.shadowRoot && el.shadowRoot.mode === 'open') {
      for (const child of el.shadowRoot.children) {
        walk(child);
      }
    }

    if (el instanceof HTMLSlotElement) {
      for (const assigned of el.assignedElements({ flatten: true })) {
        walk(assigned);
      }
    }

    for (const child of el.children) {
      walk(child);
    }
  }

  walk(root);
  return results;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any -- heterogeneous host types share only _attach/_detach
const trapStack: FocusTrapController<any>[] = [];
let scrollLockCount = 0;

export class FocusTrapController<
    Host extends ReactiveControllerHost & HTMLElement = ReactiveControllerHost & HTMLElement,
  >
  implements ReactiveController
{
  private _host: Host;
  private _getContainer: (host: Host) => HTMLElement | null | undefined;
  private _isActive: (host: Host) => boolean;
  private _lockScroll: boolean;
  private _initialFocus: 'first-focusable' | 'autofocus';

  private _boundKeydown: (e: KeyboardEvent) => void;
  private _previouslyFocused: Element | null = null;
  private _activated = false;
  private _attached = false;
  private _didLockScroll = false;
  private _rafId: number | null = null;

  constructor(host: Host, options: FocusTrapOptions<Host>) {
    this._host = host;
    this._getContainer = options.getContainer;
    this._isActive = options.isActive;
    this._lockScroll = options.lockScroll ?? false;
    this._initialFocus = options.initialFocus ?? 'first-focusable';

    this._boundKeydown = this._onKeydown.bind(this);
    host.addController(this);
  }

  hostConnected() {
    if (this._isActive(this._host)) {
      this._activate();
      this._rafId = requestAnimationFrame(() => {
        this._rafId = null;
        if (this._activated && this._host.isConnected) {
          this._focusFirstTabbable();
        }
      });
    }
  }

  hostUpdated() {
    const active = this._isActive(this._host);
    if (active && !this._activated) {
      this._activate();
      this._focusFirstTabbable();
    }
    if (!active && this._activated) {
      this._deactivate();
    }
  }

  hostDisconnected() {
    this._cancelPendingFocus();
    if (this._activated) {
      this._deactivate();
    }
  }

  private _activate() {
    if (this._activated) return;
    this._activated = true;
    this._previouslyFocused = document.activeElement;

    if (trapStack.length > 0) {
      trapStack[trapStack.length - 1]._detach();
    }
    trapStack.push(this);

    if (this._getContainer(this._host)) {
      this._attach();
    }

    if (this._lockScroll) {
      this._lockBodyScroll();
    }
  }

  private _deactivate() {
    if (!this._activated) return;
    this._activated = false;

    this._cancelPendingFocus();
    this._detach();

    const idx = trapStack.indexOf(this);
    if (idx !== -1) {
      trapStack.splice(idx, 1);
    }
    if (trapStack.length > 0) {
      trapStack[trapStack.length - 1]._attach();
    }

    if (this._didLockScroll) {
      this._unlockBodyScroll();
    }

    const prev = this._previouslyFocused;
    if (
      prev instanceof HTMLElement &&
      document.activeElement !== prev
    ) {
      prev.focus({ preventScroll: true });
    }
  }

  private _attach() {
    if (this._attached) return;
    const container = this._getContainer(this._host);
    if (!container) return;
    container.addEventListener('keydown', this._boundKeydown);
    this._attached = true;
  }

  private _detach() {
    if (!this._attached) return;
    const container = this._getContainer(this._host);
    container?.removeEventListener('keydown', this._boundKeydown);
    this._attached = false;
  }

  private _cancelPendingFocus() {
    if (this._rafId !== null) {
      cancelAnimationFrame(this._rafId);
      this._rafId = null;
    }
  }

  private _focusFirstTabbable() {
    const container = this._getContainer(this._host);
    if (!container) return;

    if (this._initialFocus === 'autofocus') {
      const autofocus = container.querySelector('[autofocus]') as HTMLElement | null;
      if (autofocus) {
        autofocus.focus();
        return;
      }
    }

    const focusable = getFocusableElements(container);
    if (focusable.length > 0) {
      focusable[0].focus();
    } else {
      container.focus();
    }
  }

  private _onKeydown(e: KeyboardEvent) {
    if (e.key !== 'Tab') return;

    const container = this._getContainer(this._host);
    if (!container) return;

    const focusable = getFocusableElements(container);
    if (focusable.length === 0) {
      e.preventDefault();
      return;
    }

    const path = e.composedPath();
    const activeIdx = focusable.findIndex((el) => path.includes(el));
    if (activeIdx === -1) return;

    const first = focusable[0];
    const last = focusable[focusable.length - 1];

    if (e.shiftKey) {
      if (focusable[activeIdx] === first) {
        e.preventDefault();
        last.focus();
      }
    } else {
      if (focusable[activeIdx] === last) {
        e.preventDefault();
        first.focus();
      }
    }
  }

  private _lockBodyScroll() {
    if (scrollLockCount === 0) {
      document.body.style.overflow = 'hidden';
    }
    scrollLockCount++;
    this._didLockScroll = true;
  }

  private _unlockBodyScroll() {
    if (this._didLockScroll) {
      scrollLockCount = Math.max(0, scrollLockCount - 1);
      if (scrollLockCount === 0) {
        document.body.style.overflow = '';
      }
      this._didLockScroll = false;
    }
  }
}
