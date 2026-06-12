import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import { toast } from '../src/internal/toast.js'
import type { SemaToaster } from '../src/lib/sema-toaster.js'

function toaster(): SemaToaster | null {
  return document.querySelector('sema-toaster') as SemaToaster | null
}
async function tick() {
  await toaster()?.updateComplete
}
async function waitFor(fn: () => unknown, timeout = 2000): Promise<void> {
  const start = performance.now()
  while (performance.now() - start < timeout) {
    if (fn()) return
    await new Promise((r) => setTimeout(r, 20))
  }
  throw new Error('waitFor timed out')
}
function toasts(): NodeListOf<Element> {
  return toaster()!.shadowRoot!.querySelectorAll('sema-toast')
}

beforeEach(() => {
  document.querySelector('sema-toaster')?.remove()
})
afterEach(() => {
  document.querySelector('sema-toaster')?.remove()
})

describe('toast() system', () => {
  it('lazily creates a singleton <sema-toaster> region and shows the message', async () => {
    expect(toaster()).toBeNull()
    toast('Hello there', { duration: null })
    await waitFor(() => toaster() !== null)
    await tick()
    const region = toaster()!.shadowRoot!.querySelector('[role="region"]')!
    expect(region.getAttribute('aria-live')).toBe('polite')
    expect(toasts().length).toBe(1)
    expect(toasts()[0].textContent).toContain('Hello there')
    // a second call reuses the same toaster
    toast('Another', { duration: null })
    await tick()
    expect(document.querySelectorAll('sema-toaster').length).toBe(1)
    expect(toasts().length).toBe(2)
  })

  it('applies the variant and the matching live-region role', async () => {
    toast.error('Boom', { duration: null })
    await waitFor(() => toasts()[0]?.getAttribute('variant') === 'error')
    expect(toasts()[0].shadowRoot!.querySelector('.toast')!.getAttribute('role')).toBe('alert')
    toast.warning('Careful', { duration: null })
    await waitFor(() => toasts()[0]?.getAttribute('variant') === 'warning')
    expect(toasts()[0].shadowRoot!.querySelector('.toast')!.getAttribute('role')).toBe('alert')
    toast.success('Yay', { duration: null })
    await waitFor(() => toasts()[0]?.getAttribute('variant') === 'success')
    expect(toasts()[0].shadowRoot!.querySelector('.toast')!.getAttribute('role')).toBe('status')
    toast.info('FYI', { duration: null })
    await waitFor(() => toasts()[0]?.getAttribute('variant') === 'info')
    expect(toasts()[0].shadowRoot!.querySelector('.toast')!.getAttribute('role')).toBe('status')
  })

  it('dismiss() removes a toast; dismissAll() clears them', async () => {
    const t = toast('one', { duration: null })
    toast('two', { duration: null })
    await waitFor(() => toasts().length === 2)
    t.dismiss()
    await tick()
    expect(toasts().length).toBe(1)
    toast.dismissAll()
    await tick()
    expect(toasts().length).toBe(0)
  })

  it('auto-dismisses after the duration', async () => {
    toast('bye soon', { duration: 60 })
    await waitFor(() => toasts().length === 1)
    await waitFor(() => toasts().length === 0)
    expect(toasts().length).toBe(0)
  })

  it('the close button dismisses a dismissible toast', async () => {
    toast('closable', { duration: null })
    await waitFor(() => toasts().length === 1)
    const close = toasts()[0].shadowRoot!.querySelector('button[part="close"]') as HTMLButtonElement
    expect(close).toBeTruthy()
    close.click()
    await tick()
    expect(toasts().length).toBe(0)
  })

  it('evicts the oldest toast beyond maxVisible', async () => {
    const el = document.createElement('sema-toaster')
    el.maxVisible = 2
    document.body.appendChild(el)
    el.show('one', { duration: null })
    el.show('two', { duration: null })
    el.show('three', { duration: null })
    await tick()
    expect(toasts().length).toBe(2)
    // newest first; 'one' was evicted
    expect(toasts()[0].textContent).toContain('three')
    expect(toasts()[1].textContent).toContain('two')
  })

  it('pauses auto-dismiss while hovered and resumes on leave', async () => {
    toast('keeps the region mounted', { duration: null })
    await waitFor(() => toasts().length === 1)
    const region = toaster()!.shadowRoot!.querySelector('.region')!
    region.dispatchEvent(new PointerEvent('pointerenter'))
    toast('hover me', { duration: 60 })
    await waitFor(() => toasts().length === 2)
    await new Promise((r) => setTimeout(r, 150))
    expect(toasts().length).toBe(2) // armed while paused — no live timer ran
    region.dispatchEvent(new PointerEvent('pointerleave'))
    await waitFor(() => toasts().length === 1)
    expect(toasts()[0].textContent).toContain('keeps the region mounted')
  })

  it('handle.update() changes the message and variant in place', async () => {
    const t = toast('Uploading…', { duration: null })
    toast('other', { duration: null })
    await waitFor(() => toasts().length === 2)
    t.update('Done', { variant: 'success', duration: null })
    await waitFor(() => toasts()[1]?.getAttribute('variant') === 'success')
    expect(toasts().length).toBe(2)
    expect(toasts()[1].textContent).toContain('Done')
    expect(toasts()[0].textContent).toContain('other')
  })
})
