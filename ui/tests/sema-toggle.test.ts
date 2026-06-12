import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-toggle.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaToggle', () => {
  it('renders with value attribute', async () => {
    document.body.innerHTML = '<sema-toggle value="tw">Tree-walker</sema-toggle>'
    const el = document.querySelector('sema-toggle')!
    expect(el.value).toBe('tw')
  })

  it('reflects selected attribute when set', async () => {
    document.body.innerHTML = '<sema-toggle value="vm" selected>Bytecode VM</sema-toggle>'
    const el = document.querySelector('sema-toggle')!
    expect(el.selected).toBe(true)
    expect(el.hasAttribute('selected')).toBe(true)
  })

  it('has aria-checked matching selected state', async () => {
    document.body.innerHTML = '<sema-toggle value="x" selected>X</sema-toggle>'
    const el = document.querySelector('sema-toggle')!
    await el.updateComplete
    const toggle = el.shadowRoot!.querySelector('.toggle')!
    expect(toggle.getAttribute('aria-checked')).toBe('true')
    expect(toggle.getAttribute('role')).toBe('radio')
  })

  it('has aria-checked false when not selected', async () => {
    document.body.innerHTML = '<sema-toggle value="x">X</sema-toggle>'
    const el = document.querySelector('sema-toggle')!
    await el.updateComplete
    const toggle = el.shadowRoot!.querySelector('.toggle')!
    expect(toggle.getAttribute('aria-checked')).toBe('false')
  })

  it('can be focused programmatically', async () => {
    document.body.innerHTML = '<sema-toggle value="x">X</sema-toggle>'
    const el = document.querySelector('sema-toggle')!
    await el.updateComplete
    el.focus()
    // focus() delegates to the internal .toggle div
    expect(el.shadowRoot!.activeElement).toBeTruthy()
  })

  it('renders slotted content as label', async () => {
    document.body.innerHTML = '<sema-toggle value="tw">Tree-walker</sema-toggle>'
    const el = document.querySelector('sema-toggle')!
    expect(el.textContent).toContain('Tree-walker')
  })
})
