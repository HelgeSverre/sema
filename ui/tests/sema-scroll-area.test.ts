import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-scroll-area.js'
import type { SemaScrollArea } from '../src/lib/sema-scroll-area.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaScrollArea', () => {
  it('exposes a viewport part and projects slotted content', async () => {
    document.body.innerHTML = `<sema-scroll-area><p>hello</p></sema-scroll-area>`
    const el = document.querySelector('sema-scroll-area') as SemaScrollArea
    await el.updateComplete
    const vp = el.shadowRoot!.querySelector('[part="viewport"]') as HTMLElement
    expect(vp).toBeTruthy()
    expect(vp.classList.contains('sema-scroll')).toBe(true)
    expect(el.querySelector('p')!.textContent).toBe('hello')
  })

  it('defaults to vertical and reflects orientation', async () => {
    document.body.innerHTML = `<sema-scroll-area></sema-scroll-area>`
    const el = document.querySelector('sema-scroll-area') as SemaScrollArea
    await el.updateComplete
    expect(el.getAttribute('orientation')).toBe('vertical')
    expect(getComputedStyle(el.shadowRoot!.querySelector('.viewport')!).overflowY).toBe('auto')

    el.orientation = 'horizontal'
    await el.updateComplete
    expect(el.getAttribute('orientation')).toBe('horizontal')
    expect(getComputedStyle(el.shadowRoot!.querySelector('.viewport')!).overflowX).toBe('auto')
  })

  // Regression: with border-box + host padding/border, the old max-height:inherit
  // viewport overflowed the host's border box and painted content outside it.
  it('keeps the viewport inside a padded, bordered, max-height host', async () => {
    document.body.innerHTML = `
      <sema-scroll-area style="box-sizing:border-box; max-height:128px; width:240px; padding:8px 12px; border:1px solid #888;">
        <div style="height:600px;">tall</div>
      </sema-scroll-area>`
    const el = document.querySelector('sema-scroll-area') as SemaScrollArea
    await el.updateComplete
    const vp = el.shadowRoot!.querySelector('.viewport') as HTMLElement
    const host = el.getBoundingClientRect()
    const view = vp.getBoundingClientRect()
    expect(host.height).toBe(128)
    // viewport bottom must not pass the host's inner (padding-box) bottom edge
    expect(view.bottom).toBeLessThanOrEqual(host.bottom - 1 - 8 + 0.5)
    // and the full content must remain reachable by scrolling
    vp.scrollTop = vp.scrollHeight
    expect(vp.scrollTop + vp.clientHeight).toBeGreaterThanOrEqual(vp.scrollHeight - 1)
  })
})
