import { beforeEach, describe, expect, it, vi } from 'vitest'
import '../src/lib/sema-tabs.js'
import type { SemaTabs, SemaTab, SemaTabPanel } from '../src/lib/sema-tabs.js'

const MARKUP = `
  <sema-tabs>
    <sema-tab value="readme">Readme</sema-tab>
    <sema-tab value="versions">Versions <span>12</span></sema-tab>
    <sema-tab value="deps" disabled>Dependencies</sema-tab>
    <sema-tab-panel value="readme">readme content</sema-tab-panel>
    <sema-tab-panel value="versions">versions content</sema-tab-panel>
    <sema-tab-panel value="deps">deps content</sema-tab-panel>
  </sema-tabs>
`

async function mount(markup = MARKUP): Promise<SemaTabs> {
  document.body.innerHTML = markup
  const tabs = document.querySelector('sema-tabs') as SemaTabs
  await tabs.updateComplete
  for (const child of tabs.querySelectorAll<SemaTab | SemaTabPanel>('sema-tab, sema-tab-panel')) {
    await child.updateComplete
  }
  // slotchange wiring lands in the same frame; flush microtasks + a tick
  await new Promise((r) => requestAnimationFrame(r))
  return tabs
}

const tabsOf = (tabs: SemaTabs) => Array.from(tabs.querySelectorAll<SemaTab>('sema-tab'))
const panelsOf = (tabs: SemaTabs) => Array.from(tabs.querySelectorAll<SemaTabPanel>('sema-tab-panel'))
const tab = (tabs: SemaTabs, value: string) => tabs.querySelector<SemaTab>(`sema-tab[value="${value}"]`)!
const panel = (tabs: SemaTabs, value: string) => tabs.querySelector<SemaTabPanel>(`sema-tab-panel[value="${value}"]`)!

// keydown dispatched from the tab HOST — it is the focusable element
function press(el: HTMLElement, key: string) {
  el.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, composed: true }))
}

beforeEach(() => {
  document.body.innerHTML = ''
  history.replaceState(null, '', window.location.pathname)
})

describe('SemaTabs — ARIA wiring (light-DOM IDREF contract)', () => {
  it('wires roles, ids, aria-selected, and resolvable aria-controls pairs', async () => {
    const el = await mount()
    const ts = tabsOf(el)
    for (const t of ts) {
      expect(t.getAttribute('role')).toBe('tab')
      expect(t.id).not.toBe('')
      const controls = t.getAttribute('aria-controls')!
      const target = document.getElementById(controls)
      expect(target).toBe(panel(el, t.value))
    }
    expect(ts.map((t) => t.getAttribute('aria-selected'))).toEqual(['true', 'false', 'false'])
    for (const p of panelsOf(el)) {
      expect(p.getAttribute('role')).toBe('tabpanel')
      expect(document.getElementById(p.getAttribute('aria-labelledby')!)).toBe(tab(el, p.value))
    }
  })

  it('preserves a user-supplied id', async () => {
    const el = await mount(`
      <sema-tabs>
        <sema-tab id="my-tab" value="a">A</sema-tab>
        <sema-tab-panel value="a">a</sema-tab-panel>
      </sema-tabs>`)
    expect(tab(el, 'a').id).toBe('my-tab')
    expect(panel(el, 'a').getAttribute('aria-labelledby')).toBe('my-tab')
  })

  it('hides inactive panels with until-found; tab without panel stays selectable', async () => {
    const el = await mount(`
      <sema-tabs>
        <sema-tab value="a">A</sema-tab>
        <sema-tab value="orphan">Orphan</sema-tab>
        <sema-tab-panel value="a">a</sema-tab-panel>
        <sema-tab-panel value="ghost">never shown</sema-tab-panel>
      </sema-tabs>`)
    expect(panel(el, 'a').hasAttribute('hidden')).toBe(false)
    expect(panel(el, 'ghost').getAttribute('hidden')).toBe('until-found')
    expect(tab(el, 'orphan').hasAttribute('aria-controls')).toBe(false)
    tab(el, 'orphan').click()
    expect(el.value).toBe('orphan')
    expect(panel(el, 'a').getAttribute('hidden')).toBe('until-found')
  })

  it('makes a content-only active panel focusable, but not one with focusables', async () => {
    const el = await mount(`
      <sema-tabs>
        <sema-tab value="text">Text</sema-tab>
        <sema-tab value="form">Form</sema-tab>
        <sema-tab-panel value="text">plain prose</sema-tab-panel>
        <sema-tab-panel value="form"><button>go</button></sema-tab-panel>
      </sema-tabs>`)
    expect(panel(el, 'text').getAttribute('tabindex')).toBe('0')
    tab(el, 'form').click()
    expect(panel(el, 'form').hasAttribute('tabindex')).toBe(false)
    expect(panel(el, 'text').hasAttribute('tabindex')).toBe(false)
  })
})

describe('SemaTabs — selection rules', () => {
  it('selects the first enabled tab by default and shows only its panel', async () => {
    const el = await mount()
    expect(el.value).toBe('readme')
    expect(tab(el, 'readme').selected).toBe(true)
    expect(panel(el, 'readme').hasAttribute('hidden')).toBe(false)
    expect(panel(el, 'versions').getAttribute('hidden')).toBe('until-found')
  })

  it('skips a disabled first tab', async () => {
    const el = await mount(`
      <sema-tabs>
        <sema-tab value="a" disabled>A</sema-tab>
        <sema-tab value="b">B</sema-tab>
        <sema-tab-panel value="a">a</sema-tab-panel>
        <sema-tab-panel value="b">b</sema-tab-panel>
      </sema-tabs>`)
    expect(el.value).toBe('b')
  })

  it('respects the value attribute, then a pre-set selected hint', async () => {
    const byValue = await mount(MARKUP.replace('<sema-tabs>', '<sema-tabs value="versions">'))
    expect(byValue.value).toBe('versions')

    const byHint = await mount(MARKUP.replace('value="versions"', 'value="versions" selected'))
    expect(byHint.value).toBe('versions')
    expect(tab(byHint, 'readme').selected).toBe(false)
  })

  it('warns and falls back on an unmatched value', async () => {
    const warn = vi.spyOn(console, 'warn').mockImplementation(() => {})
    const el = await mount(MARKUP.replace('<sema-tabs>', '<sema-tabs value="nope">'))
    expect(el.value).toBe('readme')
    expect(warn).toHaveBeenCalledOnce()
    warn.mockRestore()
  })

  it('programmatic value updates selection without emitting', async () => {
    const el = await mount()
    const seen: string[] = []
    el.addEventListener('sema-change', (e) => seen.push(e.detail.value))
    el.value = 'versions'
    await el.updateComplete
    expect(tab(el, 'versions').selected).toBe(true)
    expect(panel(el, 'versions').hasAttribute('hidden')).toBe(false)
    expect(seen).toEqual([])
  })

  it('click selects, emits once on the sema-tabs host; disabled and re-clicks do not', async () => {
    const el = await mount()
    const seen: { value: string; target: EventTarget | null }[] = []
    el.addEventListener('sema-change', (e) => seen.push({ value: e.detail.value, target: e.target }))
    tab(el, 'versions').click()
    expect(seen).toEqual([{ value: 'versions', target: el }])
    tab(el, 'versions').click() // re-activation: no-op
    tab(el, 'deps').click() // disabled: no-op
    expect(seen.length).toBe(1)
    expect(el.value).toBe('versions')
  })
})

describe('SemaTabs — keyboard', () => {
  it('keeps exactly one tab in the tab order', async () => {
    const el = await mount()
    const stops = () => tabsOf(el).map((t) => t.getAttribute('tabindex'))
    expect(stops()).toEqual(['0', '-1', '-1'])
    press(tab(el, 'readme'), 'ArrowRight')
    expect(stops()).toEqual(['-1', '0', '-1'])
  })

  it('auto mode: arrows wrap, skip disabled, select, and emit', async () => {
    const el = await mount()
    const seen: string[] = []
    el.addEventListener('sema-change', (e) => seen.push(e.detail.value))

    press(tab(el, 'readme'), 'ArrowRight')
    expect(el.value).toBe('versions')
    // deps is disabled → wraps past it back to readme
    press(tab(el, 'versions'), 'ArrowRight')
    expect(el.value).toBe('readme')
    press(tab(el, 'readme'), 'ArrowLeft')
    expect(el.value).toBe('versions')
    press(tab(el, 'versions'), 'Home')
    expect(el.value).toBe('readme')
    press(tab(el, 'readme'), 'End')
    expect(el.value).toBe('versions')
    expect(seen).toEqual([{}, {}, {}, {}, {}].map((_, i) => ['versions', 'readme', 'versions', 'readme', 'versions'][i]))
  })

  it('manual mode: arrows only move focus; Enter/Space select', async () => {
    const el = await mount(MARKUP.replace('<sema-tabs>', '<sema-tabs activation="manual">'))
    const seen: string[] = []
    el.addEventListener('sema-change', (e) => seen.push(e.detail.value))

    press(tab(el, 'readme'), 'ArrowRight')
    expect(el.value).toBe('readme')
    expect(tab(el, 'versions').getAttribute('tabindex')).toBe('0')
    expect(seen).toEqual([])

    press(tab(el, 'versions'), 'Enter')
    expect(el.value).toBe('versions')
    press(tab(el, 'versions'), 'ArrowLeft')
    press(tab(el, 'readme'), ' ')
    expect(el.value).toBe('readme')
    expect(seen).toEqual(['versions', 'readme'])
  })

  it('ArrowUp/ArrowDown are not bound', async () => {
    const el = await mount()
    press(tab(el, 'readme'), 'ArrowDown')
    press(tab(el, 'readme'), 'ArrowUp')
    expect(el.value).toBe('readme')
    expect(tab(el, 'readme').getAttribute('tabindex')).toBe('0')
  })
})

describe('SemaTabs — dynamic mutations', () => {
  it('wires a tab+panel appended at runtime', async () => {
    const el = await mount()
    const t = document.createElement('sema-tab')
    t.value = 'new'
    t.textContent = 'New'
    const p = document.createElement('sema-tab-panel')
    p.value = 'new'
    el.append(t, p)
    await new Promise((r) => requestAnimationFrame(r))

    expect(t.getAttribute('role')).toBe('tab')
    expect(document.getElementById(t.getAttribute('aria-controls')!)).toBe(p)
    press(tab(el, 'versions'), 'ArrowRight') // deps disabled → lands on new
    expect(el.value).toBe('new')
  })

  it('repairs selection silently when the active tab is removed', async () => {
    const el = await mount(MARKUP.replace('<sema-tabs>', '<sema-tabs value="versions">'))
    const seen: string[] = []
    el.addEventListener('sema-change', (e) => seen.push(e.detail.value))
    tab(el, 'versions').remove()
    await new Promise((r) => requestAnimationFrame(r))

    expect(el.value).toBe('readme')
    expect(panel(el, 'readme').hasAttribute('hidden')).toBe(false)
    expect(panel(el, 'versions').getAttribute('hidden')).toBe('until-found')
    expect(tab(el, 'readme').getAttribute('tabindex')).toBe('0')
    expect(seen).toEqual([])
  })

  it('repairs selection when the active tab becomes disabled', async () => {
    const el = await mount(MARKUP.replace('<sema-tabs>', '<sema-tabs value="versions">'))
    tab(el, 'versions').disabled = true
    await tab(el, 'versions').updateComplete
    await new Promise((r) => requestAnimationFrame(r))
    expect(el.value).toBe('readme')
    expect(tab(el, 'versions').getAttribute('aria-disabled')).toBe('true')
  })
})

describe('SemaTabs — find-in-page and hash sync', () => {
  it('beforematch on a hidden panel activates its tab and emits', async () => {
    const el = await mount()
    const seen: string[] = []
    el.addEventListener('sema-change', (e) => seen.push(e.detail.value))
    panel(el, 'versions').dispatchEvent(new Event('beforematch', { bubbles: true }))
    expect(el.value).toBe('versions')
    expect(seen).toEqual(['versions'])
  })

  it('hash-sync adopts location.hash on connect and mirrors activation', async () => {
    history.replaceState(null, '', '#versions')
    const el = await mount(MARKUP.replace('<sema-tabs>', '<sema-tabs hash-sync>'))
    expect(el.value).toBe('versions')

    tab(el, 'readme').click()
    expect(window.location.hash).toBe('#readme')
  })
})
