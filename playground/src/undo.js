export class TextareaUndo {
  constructor(textarea, { max = 200, mergeDelay = 600, onChange = null } = {}) {
    this.ta = textarea;
    this.max = max;
    this.mergeDelay = mergeDelay;
    this.onChange = onChange;
    this.stack = [this._read()];
    this.index = 0;
    this._applying = false;
    this._inTransaction = 0;
    this._suppress = false;
    this._lastInputType = null;
    this._lastPushAt = 0;
    this._lastKind = null;
    this._composing = false;
    this._forceNew = false;

    textarea.addEventListener('beforeinput', (e) => { this._lastInputType = e.inputType || null; });
    textarea.addEventListener('compositionstart', () => { this._composing = true; });
    textarea.addEventListener('compositionend', () => { this._composing = false; this._forceNew = true; });
    textarea.addEventListener('input', () => {
      if (this._applying || this._suppress || this._inTransaction || this._composing) return;
      this._record();
    });
    textarea.addEventListener('keydown', (e) => {
      const mod = e.metaKey || e.ctrlKey;
      if (mod && !e.altKey && e.key.toLowerCase() === 'z') {
        e.preventDefault();
        e.shiftKey ? this.redo() : this.undo();
      } else if (mod && !e.altKey && e.key.toLowerCase() === 'y') {
        e.preventDefault();
        this.redo();
      }
    });
  }

  _read() { return { value: this.ta.value, start: this.ta.selectionStart ?? 0, end: this.ta.selectionEnd ?? 0 }; }

  undo() { if (this.index > 0) { this.index--; this._apply(this.stack[this.index]); } }
  redo() { if (this.index < this.stack.length - 1) { this.index++; this._apply(this.stack[this.index]); } }

  transact(fn) {
    this._inTransaction++;
    try { fn(); } finally {
      this._inTransaction--;
      if (this._inTransaction === 0) this._record(true);
    }
  }

  reset() { this.stack = [this._read()]; this.index = 0; this._lastPushAt = 0; this._lastKind = null; }

  _record(forceNew = false) {
    const next = this._read();
    const cur = this.stack[this.index];
    if (cur.value === next.value && cur.start === next.start && cur.end === next.end) return;

    const now = performance.now();
    const it = this._lastInputType;
    const kind = it?.startsWith('insert') ? 'insert' : it?.startsWith('delete') ? 'delete' : 'other';
    const forcedByType = it === 'insertFromPaste' || it === 'insertFromDrop' || it === 'deleteByCut';

    let merge = false;
    if (!forceNew && !this._forceNew && !forcedByType) {
      merge = (now - this._lastPushAt) <= this.mergeDelay
        && kind === this._lastKind
        && cur.start === cur.end && next.start === next.end
        && (kind === 'insert' || kind === 'delete');
    }
    this._forceNew = false;

    if (merge) {
      this.stack[this.index] = next;
    } else {
      this.stack.splice(this.index + 1);
      this.stack.push(next);
      this.index++;
      if (this.stack.length > this.max) {
        const overflow = this.stack.length - this.max;
        this.stack.splice(0, overflow);
        this.index = Math.max(0, this.index - overflow);
      }
    }
    this._lastPushAt = now;
    this._lastKind = kind;
  }

  _apply(state) {
    this._applying = true;
    this.ta.value = state.value;
    this.ta.setSelectionRange(state.start, state.end);
    if (this.onChange) this.onChange();
    else { this._suppress = true; this.ta.dispatchEvent(new Event('input', { bubbles: true })); this._suppress = false; }
    this._applying = false;
  }
}
