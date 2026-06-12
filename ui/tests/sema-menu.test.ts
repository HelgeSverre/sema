import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-menu.js'
import type { SemaMenu } from '../src/lib/sema-menu.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

const MENU = `
  <sema-menu>
    <sema-menu-item value="a">A</sema-menu-item>
    <sema-menu-item value="b">B</sema-menu-item>
    <sema-menu-item value="c">C</sema-menu-item>
  </sema-menu>`

const MENU_WITH_DISABLED = `
  <sema-menu>
    <sema-menu-item value="a">A</sema-menu-item>
    <sema-menu-item value="b" disabled>B</sema-menu-item>
    <sema-menu-item value="c">C</sema-menu-item>
  </sema-menu>`

async function setup(markup: string): Promise<SemaMenu> {
  document.body.innerHTML = markup
  const menu = document.querySelector('sema-menu') as SemaMenu
  await menu.updateComplete
  for (const item of document.querySelectorAll('sema-menu-item')) await item.updateComplete
  return menu
}

function activeValue(): string | null | undefined {
  // delegatesFocus makes document.activeElement report the sema-menu-item host
  return (document.activeElement as HTMLElement)?.closest?.('sema-menu-item')?.getAttribute('value')
}

// The menu's keyboard handler lives on its shadow [role="menu"] div; dispatch from the
// focused item's inner .item button so the event bubbles (composed) through the slot.
function press(key: string) {
  const host = (document.activeElement as HTMLElement | null)?.closest?.('sema-menu-item')
  const item = host?.shadowRoot?.querySelector('.item') as HTMLElement
  item.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, composed: true }))
}

describe('SemaMenu — keyboard navigation', () => {
  it('ArrowDown roves focus forward and wraps at the end', async () => {
    const menu = await setup(MENU)
    menu.focusFirst()
    expect(activeValue()).toBe('a')
    press('ArrowDown')
    expect(activeValue()).toBe('b')
    press('ArrowDown')
    expect(activeValue()).toBe('c')
    press('ArrowDown')
    expect(activeValue()).toBe('a') // wraps forward
  })

  it('ArrowUp roves focus backward and wraps at the start', async () => {
    const menu = await setup(MENU)
    menu.focusFirst()
    expect(activeValue()).toBe('a')
    press('ArrowUp')
    expect(activeValue()).toBe('c') // wraps backward
    press('ArrowUp')
    expect(activeValue()).toBe('b')
  })

  it('Home and End jump to the first/last enabled item', async () => {
    const menu = await setup(MENU)
    menu.focusFirst()
    press('End')
    expect(activeValue()).toBe('c')
    press('Home')
    expect(activeValue()).toBe('a')
  })

  it('disabled items are skipped when roving', async () => {
    const menu = await setup(MENU_WITH_DISABLED)
    menu.focusFirst()
    expect(activeValue()).toBe('a')
    press('ArrowDown')
    expect(activeValue()).toBe('c') // skips the disabled item
    press('ArrowUp')
    expect(activeValue()).toBe('a')
    press('End')
    expect(activeValue()).toBe('c') // End targets the last *enabled* item
  })
})

describe('SemaMenu — selection', () => {
  it('Enter fires sema-select with the focused item', async () => {
    const menu = await setup(MENU)
    const items = document.querySelectorAll('sema-menu-item')
    const events: Array<{ value: string; item: Element }> = []
    menu.addEventListener('sema-select', (e) => events.push((e as CustomEvent).detail))
    menu.focusFirst()
    press('ArrowDown') // b
    press('Enter')
    expect(events.length).toBe(1)
    expect(events[0].value).toBe('b')
    expect(events[0].item).toBe(items[1])
  })

  it('Space fires sema-select with the focused item', async () => {
    const menu = await setup(MENU)
    const items = document.querySelectorAll('sema-menu-item')
    const events: Array<{ value: string; item: Element }> = []
    menu.addEventListener('sema-select', (e) => events.push((e as CustomEvent).detail))
    menu.focusFirst()
    press(' ')
    expect(events.length).toBe(1)
    expect(events[0].value).toBe('a')
    expect(events[0].item).toBe(items[0])
  })
})
