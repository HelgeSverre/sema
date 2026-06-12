import { beforeEach, describe, expect, it } from 'vitest'
import { LitElement, html } from 'lit'
import { property } from 'lit/decorators.js'
import { FocusTrapController } from '../src/internal/controllers/focus-trap.js'

class TestTrapHost extends LitElement {
  @property({ type: Boolean }) active = false

  trap = new FocusTrapController(this, {
    getContainer: () =>
      (this.shadowRoot?.querySelector('.container') as HTMLElement) ?? null,
    isActive: (host) => host.active,
    lockScroll: false,
  })

  render() {
    return html`
      <div class="container" tabindex="-1">
        <button class="btn1">First</button>
        <input class="mid" placeholder="middle" />
        <button class="btn2">Last</button>
      </div>
    `
  }
}

class TestAutofocusHost extends LitElement {
  @property({ type: Boolean }) active = false

  trap = new FocusTrapController(this, {
    getContainer: () =>
      (this.shadowRoot?.querySelector('.container') as HTMLElement) ?? null,
    isActive: (host) => host.active,
    initialFocus: 'autofocus',
  })

  render() {
    return html`
      <div class="container" tabindex="-1">
        <button class="btn1">First</button>
        <input class="mid" autofocus placeholder="autofocus" />
        <button class="btn2">Last</button>
      </div>
    `
  }
}

class TestEmptyHost extends LitElement {
  @property({ type: Boolean }) active = false

  trap = new FocusTrapController(this, {
    getContainer: () =>
      (this.shadowRoot?.querySelector('.container') as HTMLElement) ?? null,
    isActive: (host) => host.active,
  })

  render() {
    return html`<div class="container" tabindex="-1"><p>no focusable</p></div>`
  }
}

class TestScrollLockHost extends LitElement {
  @property({ type: Boolean }) active = false

  trap = new FocusTrapController(this, {
    getContainer: () =>
      (this.shadowRoot?.querySelector('.container') as HTMLElement) ?? null,
    isActive: (host) => host.active,
    lockScroll: true,
  })

  render() {
    return html`
      <div class="container" tabindex="-1">
        <button>Btn</button>
      </div>
    `
  }
}

customElements.define('test-trap-host', TestTrapHost)
customElements.define('test-autofocus-host', TestAutofocusHost)
customElements.define('test-empty-host', TestEmptyHost)
customElements.define('test-scroll-lock-host', TestScrollLockHost)

beforeEach(() => {
  document.body.innerHTML = ''
  document.body.style.overflow = ''
})

function getContainer(el: LitElement): HTMLElement {
  return el.shadowRoot!.querySelector('.container') as HTMLElement
}

function getBtn1(el: LitElement): HTMLElement {
  return el.shadowRoot!.querySelector('.btn1') as HTMLElement
}

function getBtn2(el: LitElement): HTMLElement {
  return el.shadowRoot!.querySelector('.btn2') as HTMLElement
}

function getInput(el: LitElement): HTMLElement {
  return el.shadowRoot!.querySelector('.mid') as HTMLElement
}

function focusedIn(el: LitElement): Element | null {
  return el.shadowRoot!.activeElement
}

describe('FocusTrapController', () => {
  // --- Initial focus ---

  it('focuses first focusable element when activated', async () => {
    document.body.innerHTML = '<test-trap-host></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete

    el.active = true
    await el.updateComplete

    expect(focusedIn(el)).toBe(getBtn1(el))
  })

  it('supports initialFocus: autofocus', async () => {
    document.body.innerHTML = '<test-autofocus-host></test-autofocus-host>'
    const el = document.querySelector('test-autofocus-host')!
    await el.updateComplete

    el.active = true
    await el.updateComplete

    expect(focusedIn(el)).toBe(getInput(el))
  })

  it('falls back to first focusable when autofocus element is absent', async () => {
    document.body.innerHTML = '<test-trap-host></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete

    el.active = true
    await el.updateComplete

    expect(focusedIn(el)).toBe(getBtn1(el))
  })

  it('focuses container when no focusable children exist', async () => {
    document.body.innerHTML = '<test-empty-host></test-empty-host>'
    const el = document.querySelector('test-empty-host')!
    await el.updateComplete

    el.active = true
    await el.updateComplete

    expect(focusedIn(el)).toBe(getContainer(el))
  })

  // --- Focus restore ---

  it('returns focus to previously focused element on deactivate', async () => {
    document.body.innerHTML =
      '<button id="outer">Outside</button><test-trap-host></test-trap-host>'
    const outer = document.querySelector('#outer') as HTMLElement
    outer.focus()

    const el = document.querySelector('test-trap-host')!
    await el.updateComplete
    el.active = true
    await el.updateComplete

    expect(focusedIn(el)).toBe(getBtn1(el))

    el.active = false
    await el.updateComplete

    expect(document.activeElement).toBe(outer)
  })

  // --- Tab wrapping ---

  it('wraps Tab from last focusable to first', async () => {
    document.body.innerHTML = '<test-trap-host></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete
    el.active = true
    await el.updateComplete

    getBtn2(el).focus()
    expect(focusedIn(el)).toBe(getBtn2(el))

    getBtn2(el).dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Tab', bubbles: true }),
    )

    expect(focusedIn(el)).toBe(getBtn1(el))
  })

  it('wraps Shift+Tab from first focusable to last', async () => {
    document.body.innerHTML = '<test-trap-host></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete
    el.active = true
    await el.updateComplete

    getBtn1(el).focus()
    expect(focusedIn(el)).toBe(getBtn1(el))

    getBtn1(el).dispatchEvent(
      new KeyboardEvent('keydown', {
        key: 'Tab',
        shiftKey: true,
        bubbles: true,
      }),
    )

    expect(focusedIn(el)).toBe(getBtn2(el))
  })

  it('prevents Tab default when no focusable elements exist', async () => {
    document.body.innerHTML = '<test-empty-host></test-empty-host>'
    const el = document.querySelector('test-empty-host')!
    await el.updateComplete
    el.active = true
    await el.updateComplete

    const event = new KeyboardEvent('keydown', {
      key: 'Tab',
      bubbles: true,
      cancelable: true,
    })
    getContainer(el).dispatchEvent(event)

    expect(event.defaultPrevented).toBe(true)
  })

  it('does not trap when inactive', async () => {
    document.body.innerHTML = '<test-trap-host></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete

    getBtn2(el).focus()
    getBtn2(el).dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Tab', bubbles: true }),
    )

    expect(focusedIn(el)).toBe(getBtn2(el))
  })

  it('removes keydown listener on deactivate', async () => {
    document.body.innerHTML = '<test-trap-host></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete
    el.active = true
    await el.updateComplete

    el.active = false
    await el.updateComplete

    getBtn2(el).focus()
    getBtn2(el).dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Tab', bubbles: true }),
    )

    expect(focusedIn(el)).toBe(getBtn2(el))
  })

  // --- Scroll lock ---

  it('locks body scroll when lockScroll is true', async () => {
    document.body.innerHTML = '<test-scroll-lock-host></test-scroll-lock-host>'
    const el = document.querySelector('test-scroll-lock-host')!
    await el.updateComplete

    el.active = true
    await el.updateComplete

    expect(document.body.style.overflow).toBe('hidden')

    el.active = false
    await el.updateComplete

    expect(document.body.style.overflow).toBe('')
  })

  it('supports nested scroll lock without premature unlock', async () => {
    document.body.innerHTML =
      '<test-scroll-lock-host id="a"></test-scroll-lock-host><test-scroll-lock-host id="b"></test-scroll-lock-host>'
    const a = document.querySelector('#a') as TestScrollLockHost
    const b = document.querySelector('#b') as TestScrollLockHost
    await a.updateComplete
    await b.updateComplete

    a.active = true
    await a.updateComplete
    expect(document.body.style.overflow).toBe('hidden')

    b.active = true
    await b.updateComplete
    expect(document.body.style.overflow).toBe('hidden')

    b.active = false
    await b.updateComplete
    expect(document.body.style.overflow).toBe('hidden')

    a.active = false
    await a.updateComplete
    expect(document.body.style.overflow).toBe('')
  })

  // --- Nested traps ---

  it('pauses outer trap when inner trap activates', async () => {
    document.body.innerHTML = `
      <test-trap-host id="outer"></test-trap-host>
      <test-trap-host id="inner"></test-trap-host>
    `
    const outer = document.querySelector('#outer') as TestTrapHost
    const inner = document.querySelector('#inner') as TestTrapHost
    await outer.updateComplete
    await inner.updateComplete

    outer.active = true
    await outer.updateComplete
    expect(focusedIn(outer)).toBe(getBtn1(outer))

    // Activate inner trap - should pause outer
    inner.active = true
    await inner.updateComplete
    expect(focusedIn(inner)).toBe(getBtn1(inner))

    // Tab in inner should wrap within inner, not outer
    getBtn2(inner).focus()
    getBtn2(inner).dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Tab', bubbles: true }),
    )
    expect(focusedIn(inner)).toBe(getBtn1(inner))

    // Deactivate inner - outer should resume
    inner.active = false
    await inner.updateComplete

    // Outer trap should be back in control
    getBtn2(outer).focus()
    getBtn2(outer).dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Tab', bubbles: true }),
    )
    expect(focusedIn(outer)).toBe(getBtn1(outer))
  })

  // --- Cleanup on disconnect ---

  it('cleans up when host is disconnected while active', async () => {
    document.body.innerHTML = '<test-scroll-lock-host></test-scroll-lock-host>'
    const el = document.querySelector('test-scroll-lock-host')!
    await el.updateComplete
    el.active = true
    await el.updateComplete

    expect(document.body.style.overflow).toBe('hidden')

    el.remove()

    expect(document.body.style.overflow).toBe('')
  })

  // --- Edge cases ---

  it('works when created with active attribute', async () => {
    document.body.innerHTML = '<test-trap-host active></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete
    // Focus is deferred via requestAnimationFrame on initial connect
    await new Promise((r) => requestAnimationFrame(r))

    expect(focusedIn(el)).toBe(getBtn1(el))
  })

  it('does not refocus on unrelated updates', async () => {
    document.body.innerHTML = '<test-trap-host></test-trap-host>'
    const el = document.querySelector('test-trap-host')!
    await el.updateComplete
    el.active = true
    await el.updateComplete

    getBtn2(el).focus()
    expect(focusedIn(el)).toBe(getBtn2(el))

    // Trigger an unrelated re-render
    el.requestUpdate()
    await el.updateComplete

    // Focus should NOT have been stolen back to btn1
    expect(focusedIn(el)).toBe(getBtn2(el))
  })
})
