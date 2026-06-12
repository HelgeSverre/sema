import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-popover.js'
import '../src/lib/sema-menu.js'
import '../src/lib/sema-button.js'
import type { SemaPopover } from '../src/lib/sema-popover.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

const MENU = `
  <sema-popover>
    <button slot="trigger">Open</button>
    <sema-menu>
      <sema-menu-item value="a">A</sema-menu-item>
      <sema-menu-item value="b">B</sema-menu-item>
    </sema-menu>
  </sema-popover>`

function pop(): SemaPopover {
  return document.querySelector('sema-popover') as SemaPopover
}
function panel(el: SemaPopover) {
  return el.shadowRoot!.querySelector('.panel') as HTMLElement
}
async function waitFor(fn: () => unknown, timeout = 2000): Promise<void> {
  const start = performance.now()
  while (performance.now() - start < timeout) {
    if (fn()) return
    await new Promise((r) => setTimeout(r, 20))
  }
  throw new Error('waitFor timed out')
}
function activeItemValue(): string | null | undefined {
  return (document.activeElement as HTMLElement)?.closest?.('sema-menu-item')?.getAttribute('value')
}
// The menu's keyboard handler lives on its shadow [role="menu"] div; dispatch from the
// focused item's inner .item button so the event bubbles (composed) through the slot.
function pressActiveItem(key: string) {
  const host = (document.activeElement as HTMLElement | null)?.closest?.('sema-menu-item')
  const item = host?.shadowRoot?.querySelector('.item') as HTMLElement
  item.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, composed: true }))
}

describe('SemaPopover', () => {
  it('is closed initially and opens on trigger click', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    await el.updateComplete
    expect(el.open).toBe(false)
    expect(panel(el).hasAttribute('hidden')).toBe(true)
    ;(el.querySelector('[slot="trigger"]') as HTMLElement).click()
    await el.updateComplete
    expect(el.open).toBe(true)
    expect(panel(el).hasAttribute('hidden')).toBe(false)
  })

  it('closes on outside pointerdown', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    expect(el.open).toBe(true)
    document.body.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true, composed: true }))
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('closes on Escape', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    panel(el).dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('emits sema-open / sema-close', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    await el.updateComplete
    let opened = 0
    let closed = 0
    el.addEventListener('sema-open', () => opened++)
    el.addEventListener('sema-close', () => closed++)
    el.show()
    el.hide()
    expect(opened).toBe(1)
    expect(closed).toBe(1)
  })

  it('a child menu selection emits sema-select and closes the popover', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    let selected = ''
    el.addEventListener('sema-select', (e) => {
      selected = (e as CustomEvent).detail.value
    })
    const itemB = el.querySelectorAll('sema-menu-item')[1] as HTMLElement
    itemB.shadowRoot!.querySelector('button')!.click()
    await el.updateComplete
    expect(selected).toBe('b')
    expect(el.open).toBe(false)
  })

  it('moves focus to the first menu item on open', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    await waitFor(() => activeItemValue() === 'a')
    expect(activeItemValue()).toBe('a')
  })

  it('ArrowDown/ArrowUp rove focus through the open menu with wrap', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    await waitFor(() => activeItemValue() === 'a')
    pressActiveItem('ArrowDown')
    expect(activeItemValue()).toBe('b')
    pressActiveItem('ArrowDown') // wraps past the end
    expect(activeItemValue()).toBe('a')
    pressActiveItem('ArrowUp') // wraps backward
    expect(activeItemValue()).toBe('b')
  })

  it('Escape closes and returns focus to the trigger', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    await waitFor(() => activeItemValue() === 'a')
    panel(el).dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))
    await el.updateComplete
    expect(el.open).toBe(false)
    expect(document.activeElement).toBe(el.querySelector('[slot="trigger"]'))
  })

  it('Tab closes a (non-modal) popover', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    panel(el).dispatchEvent(new KeyboardEvent('keydown', { key: 'Tab', bubbles: true }))
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('closes when focus leaves the popover', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    el.show()
    await el.updateComplete
    panel(el).dispatchEvent(
      new FocusEvent('focusout', { relatedTarget: document.body, bubbles: true }),
    )
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('sets aria-haspopup and toggles aria-expanded on the trigger', async () => {
    document.body.innerHTML = MENU
    const el = pop()
    const trigger = el.querySelector('[slot="trigger"]') as HTMLElement
    el.show()
    await el.updateComplete
    expect(trigger.getAttribute('aria-expanded')).toBe('true')
    expect(trigger.getAttribute('aria-haspopup')).toBe('menu')
    el.hide()
    await el.updateComplete
    expect(trigger.getAttribute('aria-expanded')).toBe('false')
  })

  it('modal popover does not close on Tab — it wraps focus inside the panel', async () => {
    document.body.innerHTML = `
      <sema-popover modal>
        <button slot="trigger">Open</button>
        <div><button id="first">one</button><button id="last">two</button></div>
      </sema-popover>`
    const el = pop()
    el.show()
    await el.updateComplete
    const first = document.querySelector('#first') as HTMLButtonElement
    const last = document.querySelector('#last') as HTMLButtonElement

    // The trap listens on the shadow panel; dispatch from the focused button so the
    // composed event's path includes it (that's how the trap finds the active element).
    last.focus()
    last.dispatchEvent(new KeyboardEvent('keydown', { key: 'Tab', bubbles: true, composed: true }))
    expect(el.open).toBe(true)
    expect(document.activeElement).toBe(first) // wrapped to the first

    first.dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Tab', shiftKey: true, bubbles: true, composed: true }),
    )
    expect(el.open).toBe(true)
    expect(document.activeElement).toBe(last) // wrapped back to the last
  })
})

describe('SemaMenu', () => {
  it('focusFirst focuses the first enabled item', async () => {
    document.body.innerHTML = `
      <sema-menu>
        <sema-menu-item value="a" disabled>A</sema-menu-item>
        <sema-menu-item value="b">B</sema-menu-item>
      </sema-menu>`
    const menu = document.querySelector('sema-menu') as HTMLElement & { focusFirst(): void }
    await (menu as unknown as { updateComplete: Promise<unknown> }).updateComplete
    menu.focusFirst()
    const focused = document.activeElement as HTMLElement
    // focus lands on the enabled item ("B"), whose host is the second menu-item
    expect(focused.closest('sema-menu-item')?.getAttribute('value')).toBe('b')
  })
})
