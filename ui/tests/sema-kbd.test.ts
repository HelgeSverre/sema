import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-kbd.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaKbd', () => {
  it('renders a single key from slotted content', async () => {
    document.body.innerHTML = '<sema-kbd>Esc</sema-kbd>'
    const el = document.querySelector('sema-kbd')!
    await el.updateComplete
    const caps = el.shadowRoot!.querySelectorAll('kbd')
    expect(caps.length).toBe(1)
    expect(el.textContent).toContain('Esc')
  })

  it('splits a combo from the keys attribute into one cap per key', async () => {
    document.body.innerHTML = '<sema-kbd keys="Cmd+Shift+P"></sema-kbd>'
    const el = document.querySelector('sema-kbd')!
    await el.updateComplete
    const caps = [...el.shadowRoot!.querySelectorAll('kbd')].map((k) => k.textContent)
    expect(caps).toEqual(['Cmd', 'Shift', 'P'])
  })

  it('renders separators between caps but not before the first', async () => {
    document.body.innerHTML = '<sema-kbd keys="Ctrl+K"></sema-kbd>'
    const el = document.querySelector('sema-kbd')!
    await el.updateComplete
    const seps = el.shadowRoot!.querySelectorAll('.sep')
    expect(seps.length).toBe(1)
    expect(seps[0].getAttribute('aria-hidden')).toBe('true')
  })

  it('trims whitespace and drops empty segments', async () => {
    document.body.innerHTML = '<sema-kbd keys=" Cmd + K "></sema-kbd>'
    const el = document.querySelector('sema-kbd')!
    await el.updateComplete
    const caps = [...el.shadowRoot!.querySelectorAll('kbd')].map((k) => k.textContent)
    expect(caps).toEqual(['Cmd', 'K'])
  })

  it('exposes a "key" part on every cap', async () => {
    document.body.innerHTML = '<sema-kbd keys="A+B"></sema-kbd>'
    const el = document.querySelector('sema-kbd')!
    await el.updateComplete
    expect(el.shadowRoot!.querySelectorAll('[part="key"]').length).toBe(2)
  })
})
