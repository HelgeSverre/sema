import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-tooltip.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

async function waitFor(fn: () => unknown, timeout = 2000): Promise<void> {
  const start = performance.now()
  while (performance.now() - start < timeout) {
    if (fn()) return
    await new Promise((r) => setTimeout(r, 20))
  }
  throw new Error('waitFor timed out')
}

describe('SemaTooltip', () => {
  it('renders tooltip with content', async () => {
    document.body.innerHTML = `
      <sema-tooltip content="Hello tooltip" placement="top">
        <button>Trigger</button>
      </sema-tooltip>`
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const tip = el.shadowRoot!.querySelector('.tooltip')
    expect(tip).toBeTruthy()
    expect(tip!.textContent).toContain('Hello tooltip')
  })

  it('does not render tooltip div when content is empty', async () => {
    document.body.innerHTML = `
      <sema-tooltip content="" placement="top">
        <button>Trigger</button>
      </sema-tooltip>`
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const tip = el.shadowRoot!.querySelector('.tooltip')
    expect(tip).toBeNull()
  })

  it('reflects placement attribute', async () => {
    document.body.innerHTML = '<sema-tooltip content="test" placement="bottom"></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    expect(el.getAttribute('placement')).toBe('bottom')
  })

  it('defaults to top placement', async () => {
    document.body.innerHTML = '<sema-tooltip content="test"></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    expect(el.placement).toBe('top')
  })

  it('has tooltip role and accessible name on the tip div', async () => {
    document.body.innerHTML = '<sema-tooltip content="test" placement="top"><button>T</button></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const tip = el.shadowRoot!.querySelector('.tooltip')!
    expect(tip.getAttribute('role')).toBe('tooltip')
    expect(tip.getAttribute('aria-label')).toBe('test')
  })

  it('renders arrow element inside tooltip', async () => {
    document.body.innerHTML = '<sema-tooltip content="test" placement="top"><button>T</button></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const arrow = el.shadowRoot!.querySelector('.tooltip-arrow')
    expect(arrow).toBeTruthy()
  })

  it('applies aria-description to the slotted trigger', async () => {
    document.body.innerHTML = `
      <sema-tooltip content="Describes the button">
        <button>Trigger</button>
      </sema-tooltip>`
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const btn = el.querySelector('button')!
    await waitFor(() => btn.getAttribute('aria-description') === 'Describes the button')
  })

  it('updates and removes aria-description when content changes', async () => {
    document.body.innerHTML = '<sema-tooltip content="Before"><button>T</button></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const btn = el.querySelector('button')!
    await waitFor(() => btn.getAttribute('aria-description') === 'Before')
    el.content = 'After'
    await el.updateComplete
    expect(btn.getAttribute('aria-description')).toBe('After')
    el.content = ''
    await el.updateComplete
    expect(btn.hasAttribute('aria-description')).toBe(false)
  })

  it('moves aria-description to the new trigger when the slotted child is swapped', async () => {
    document.body.innerHTML = '<sema-tooltip content="Tip"><button id="a">A</button></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const first = el.querySelector('#a')!
    await waitFor(() => first.getAttribute('aria-description') === 'Tip')
    const second = document.createElement('button')
    second.id = 'b'
    second.textContent = 'B'
    first.replaceWith(second)
    await waitFor(() => second.getAttribute('aria-description') === 'Tip')
    expect(first.hasAttribute('aria-description')).toBe(false)
  })

  it('strips aria-description on disconnect and re-applies it on reconnect', async () => {
    document.body.innerHTML = '<sema-tooltip content="Tip"><button>T</button></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const btn = el.querySelector('button')!
    await waitFor(() => btn.getAttribute('aria-description') === 'Tip')
    el.remove()
    expect(btn.hasAttribute('aria-description')).toBe(false)
    document.body.appendChild(el)
    await waitFor(() => btn.getAttribute('aria-description') === 'Tip')
  })

  it('blurs the focused trigger on Escape so the tooltip hides', async () => {
    document.body.innerHTML = '<sema-tooltip content="Tip"><button>T</button></sema-tooltip>'
    const el = document.querySelector('sema-tooltip')!
    await el.updateComplete
    const btn = el.querySelector('button')!
    btn.focus()
    expect(el.matches(':focus-within')).toBe(true)
    btn.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    expect(document.activeElement).not.toBe(btn)
    expect(el.matches(':focus-within')).toBe(false)
  })
})
