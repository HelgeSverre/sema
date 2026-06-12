import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-spinner.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaSpinner', () => {
  it('defaults to the md size', async () => {
    document.body.innerHTML = '<sema-spinner></sema-spinner>'
    const el = document.querySelector('sema-spinner')!
    expect(el.size).toBe('md')
  })

  it('exposes a status live region with the default label', async () => {
    document.body.innerHTML = '<sema-spinner></sema-spinner>'
    const el = document.querySelector('sema-spinner')!
    await el.updateComplete
    const status = el.shadowRoot!.querySelector('[role="status"]')!
    expect(status).toBeTruthy()
    expect(status.textContent).toBe('Loading')
  })

  it('uses a custom label', async () => {
    document.body.innerHTML = '<sema-spinner label="Compiling"></sema-spinner>'
    const el = document.querySelector('sema-spinner')!
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('[role="status"]')!.textContent).toBe('Compiling')
  })

  it('hides the visual ring from assistive tech', async () => {
    document.body.innerHTML = '<sema-spinner></sema-spinner>'
    const el = document.querySelector('sema-spinner')!
    await el.updateComplete
    const ring = el.shadowRoot!.querySelector('[part="spinner"]')!
    expect(ring.getAttribute('aria-hidden')).toBe('true')
  })

  it('reflects size to an attribute for CSS sizing', async () => {
    document.body.innerHTML = '<sema-spinner size="lg"></sema-spinner>'
    const el = document.querySelector('sema-spinner')!
    await el.updateComplete
    expect(el.getAttribute('size')).toBe('lg')
  })
})
