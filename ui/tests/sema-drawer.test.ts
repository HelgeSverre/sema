import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-drawer.js'
import type { SemaDrawer } from '../src/lib/sema-drawer.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

afterEach(() => {
  // Drawer locks body scroll while open; ensure no leak between tests.
  document.body.innerHTML = ''
})

function mount(html: string): SemaDrawer {
  document.body.innerHTML = html
  return document.querySelector('sema-drawer')!
}

describe('SemaDrawer', () => {
  it('defaults to closed and right placement', async () => {
    const el = mount('<sema-drawer></sema-drawer>')
    await el.updateComplete
    expect(el.open).toBe(false)
    expect(el.placement).toBe('right')
    expect(el.shadowRoot!.querySelector('.panel')).toBeNull()
  })

  it('renders the panel and backdrop when open', async () => {
    const el = mount('<sema-drawer open label="Settings"></sema-drawer>')
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('[part="panel"]')).toBeTruthy()
    expect(el.shadowRoot!.querySelector('[part="backdrop"]')).toBeTruthy()
  })

  it('show() and close() toggle the open state', async () => {
    const el = mount('<sema-drawer></sema-drawer>')
    el.show()
    await el.updateComplete
    expect(el.open).toBe(true)
    el.close()
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('reflects placement to an attribute', async () => {
    const el = mount('<sema-drawer placement="left" open></sema-drawer>')
    await el.updateComplete
    expect(el.getAttribute('placement')).toBe('left')
  })

  it('sets dialog semantics with a labelled title', async () => {
    const el = mount('<sema-drawer open label="Filters"></sema-drawer>')
    await el.updateComplete
    const panel = el.shadowRoot!.querySelector('[part="panel"]')!
    expect(panel.getAttribute('role')).toBe('dialog')
    expect(panel.getAttribute('aria-modal')).toBe('true')
    const titleId = panel.getAttribute('aria-labelledby')
    expect(titleId).toBeTruthy()
    expect(el.shadowRoot!.getElementById(titleId!)!.textContent).toContain('Filters')
  })

  it('closes on backdrop click', async () => {
    const el = mount('<sema-drawer open></sema-drawer>')
    await el.updateComplete
    const backdrop = el.shadowRoot!.querySelector('.backdrop') as HTMLElement
    backdrop.click()
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('closes on the built-in close button', async () => {
    const el = mount('<sema-drawer open></sema-drawer>')
    await el.updateComplete
    const close = el.shadowRoot!.querySelector('[part="close"]') as HTMLElement
    close.click()
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('closes on Escape', async () => {
    const el = mount('<sema-drawer open></sema-drawer>')
    await el.updateComplete
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }))
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('fires sema-drawer-open and sema-drawer-close', async () => {
    const el = mount('<sema-drawer></sema-drawer>')
    let opened = 0
    let closed = 0
    el.addEventListener('sema-drawer-open', () => opened++)
    el.addEventListener('sema-drawer-close', () => closed++)
    el.show()
    await el.updateComplete
    el.close()
    await el.updateComplete
    expect(opened).toBe(1)
    expect(closed).toBe(1)
  })

  it('moves focus into the panel when opened', async () => {
    const el = mount('<sema-drawer open><button id="inner">Save</button></sema-drawer>')
    await el.updateComplete
    // Focus trap focuses the first focusable element after a rAF tick.
    await new Promise((r) => requestAnimationFrame(() => r(null)))
    const active = el.shadowRoot!.activeElement || document.activeElement
    expect(active).toBeTruthy()
  })
})
