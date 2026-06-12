import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-badge.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaBadge', () => {
  it('defaults to the neutral variant', async () => {
    document.body.innerHTML = '<sema-badge>v0.1</sema-badge>'
    const el = document.querySelector('sema-badge')!
    expect(el.variant).toBe('neutral')
  })

  it('renders slotted content', async () => {
    document.body.innerHTML = '<sema-badge>beta</sema-badge>'
    const el = document.querySelector('sema-badge')!
    expect(el.textContent).toContain('beta')
  })

  it('reflects variant to an attribute for CSS selectors', async () => {
    document.body.innerHTML = '<sema-badge variant="success">ok</sema-badge>'
    const el = document.querySelector('sema-badge')!
    await el.updateComplete
    expect(el.getAttribute('variant')).toBe('success')
  })

  it('exposes a "badge" part', async () => {
    document.body.innerHTML = '<sema-badge>x</sema-badge>'
    const el = document.querySelector('sema-badge')!
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('[part="badge"]')).toBeTruthy()
  })

  it('renders a status dot only when [dot] is set', async () => {
    document.body.innerHTML = '<sema-badge>a</sema-badge><sema-badge dot>b</sema-badge>'
    const [plain, dotted] = [...document.querySelectorAll('sema-badge')]
    await plain.updateComplete
    await dotted.updateComplete
    expect(plain.shadowRoot!.querySelector('.dot')).toBeNull()
    expect(dotted.shadowRoot!.querySelector('.dot')).toBeTruthy()
  })

  it('hides the dot from assistive tech', async () => {
    document.body.innerHTML = '<sema-badge dot>b</sema-badge>'
    const el = document.querySelector('sema-badge')!
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('.dot')!.getAttribute('aria-hidden')).toBe('true')
  })

  it('applies the pill shape via the [pill] attribute', async () => {
    document.body.innerHTML = '<sema-badge pill>anthropic</sema-badge>'
    const el = document.querySelector('sema-badge')!
    await el.updateComplete
    expect(el.pill).toBe(true)
    expect(el.hasAttribute('pill')).toBe(true)
  })
})
