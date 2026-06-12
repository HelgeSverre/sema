import { beforeEach, describe, expect, it } from 'vitest'
import { page } from 'vitest/browser'
import '../src/lib/sema-button.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaButton', () => {
  it('renders with variant attribute reflected to host', async () => {
    document.body.innerHTML = '<sema-button variant="primary">Click</sema-button>'
    const el = document.querySelector('sema-button')!
    expect(el.getAttribute('variant')).toBe('primary')
  })

  it('renders internal button with type="button"', async () => {
    document.body.innerHTML = '<sema-button variant="primary">Click</sema-button>'
    await expect.element(page.getByRole('button')).toHaveAttribute('type', 'button')
  })

  it('disables internal button when disabled attribute is set', async () => {
    document.body.innerHTML = '<sema-button variant="primary" disabled>Click</sema-button>'
    const el = document.querySelector('sema-button')!
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('button')!.disabled).toBe(true)
  })

  it('renders all variant attributes correctly', async () => {
    const variants = ['primary', 'secondary', 'ghost', 'icon', 'pill', 'run', 'debug', 'action']
    for (const v of variants) {
      document.body.innerHTML = `<sema-button variant="${v}">x</sema-button>`
      const el = document.querySelector('sema-button')!
      expect(el.getAttribute('variant')).toBe(v)
    }
  })

  it('renders shortcut badge when shortcut attribute is set', async () => {
    document.body.innerHTML = '<sema-button variant="run" shortcut="⌘↵">Run</sema-button>'
    const el = document.querySelector('sema-button')!
    await el.updateComplete
    const badge = el.shadowRoot!.querySelector('.shortcut')
    expect(badge).toBeTruthy()
    expect(badge!.textContent).toContain('⌘')
  })

  it('does not render shortcut badge without shortcut attribute', async () => {
    document.body.innerHTML = '<sema-button variant="run">Run</sema-button>'
    await document.querySelector('sema-button')!.updateComplete
    const el = document.querySelector('sema-button')!
    const badge = el.shadowRoot!.querySelector('.shortcut')
    expect(badge).toBeNull()
  })

  it('forwards aria-label from host to internal button', async () => {
    document.body.innerHTML = '<sema-button variant="icon" aria-label="Close dialog">×</sema-button>'
    const el = document.querySelector('sema-button')!
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('button')!.getAttribute('aria-label')).toBe('Close dialog')
  })

  it('reflects danger attribute on host', async () => {
    document.body.innerHTML = '<sema-button variant="debug" danger>⬛</sema-button>'
    const el = document.querySelector('sema-button')!
    expect(el.hasAttribute('danger')).toBe(true)
  })

  it('defaults to primary variant when none specified', async () => {
    document.body.innerHTML = '<sema-button>Default</sema-button>'
    const el = document.querySelector('sema-button')!
    expect(el.variant).toBe('primary')
  })
})
