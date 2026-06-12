import type { ReactiveController, ReactiveControllerHost } from 'lit';

/**
 * Copy-to-clipboard controller with transient "Copied" feedback.
 *
 * Owns the clipboard write + a `copied` flag that resets after `resetMs`, requesting
 * a host update on each change. Justified as a controller (vs an inline handler)
 * because it manages a timer + reactive state and is shared by <sema-code> and
 * <sema-terminal>.
 *
 * ```ts
 * private _copy = new CopyController(this, () => this._code);
 * // render: <button class=${this._copy.copied ? 'copied' : ''} @click=${this._copy.copy}>
 * //           ${this._copy.copied ? 'Copied' : 'Copy'}</button>
 * ```
 */
export class CopyController implements ReactiveController {
  private _host: ReactiveControllerHost;
  private _getText: () => string;
  private _resetMs: number;
  private _timer: ReturnType<typeof setTimeout> | null = null;

  /** True for `resetMs` after a successful copy. */
  copied = false;

  constructor(host: ReactiveControllerHost, getText: () => string, resetMs = 1500) {
    this._host = host;
    this._getText = getText;
    this._resetMs = resetMs;
    host.addController(this);
  }

  hostDisconnected() {
    this._clear();
  }

  copy = async () => {
    try {
      await navigator.clipboard.writeText(this._getText());
    } catch {
      return; // clipboard unavailable — no feedback
    }
    this.copied = true;
    this._host.requestUpdate();
    this._clear();
    this._timer = setTimeout(() => {
      this.copied = false;
      this._timer = null;
      this._host.requestUpdate();
    }, this._resetMs);
  };

  private _clear() {
    if (this._timer) {
      clearTimeout(this._timer);
      this._timer = null;
    }
  }
}
