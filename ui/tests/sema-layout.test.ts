import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-container.js'
import '../src/lib/sema-grid.js'
import '../src/lib/sema-sidebar.js'
import type { SemaGrid } from '../src/lib/sema-grid.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

/** Poll until `fn()` is truthy (container-query / wrap reflow settling). */
async function waitFor(fn: () => unknown, timeout = 2000): Promise<void> {
  const start = performance.now()
  while (performance.now() - start < timeout) {
    if (fn()) return
    await new Promise((r) => setTimeout(r, 20))
  }
  throw new Error('waitFor timed out')
}

/** Used track sizes of the shadow grid wrapper (resolved grid-template-columns). */
function tracks(el: SemaGrid): number[] {
  const grid = el.shadowRoot!.querySelector('[part="grid"]')!
  return getComputedStyle(grid)
    .gridTemplateColumns.split(' ')
    .map((t) => parseFloat(t))
}

/** Expected auto-fill track count: floor((width + gap) / (min + gap)). */
function autoFillCount(width: number, min: number, gap: number): number {
  return Math.floor((width + gap) / (min + gap))
}

describe('layout primitives — registration and reflected-attribute defaults', () => {
  it('defines all three elements', () => {
    expect(customElements.get('sema-container')).toBeTruthy()
    expect(customElements.get('sema-grid')).toBeTruthy()
    expect(customElements.get('sema-sidebar')).toBeTruthy()
  })

  it('bare elements carry no attributes (defaults never reflect)', async () => {
    document.body.innerHTML =
      '<sema-container>a</sema-container><sema-grid><div>b</div></sema-grid>' +
      '<sema-sidebar><div slot="aside">c</div><div>d</div></sema-sidebar>'
    const container = document.querySelector('sema-container')!
    const grid = document.querySelector('sema-grid')!
    const sidebar = document.querySelector('sema-sidebar')!
    await Promise.all([container.updateComplete, grid.updateComplete, sidebar.updateComplete])
    expect(container.hasAttribute('size')).toBe(false)
    expect(container.hasAttribute('gutter')).toBe(false)
    expect(grid.hasAttribute('min')).toBe(false)
    expect(grid.hasAttribute('cols')).toBe(false)
    expect(grid.hasAttribute('gap')).toBe(false)
    expect(sidebar.hasAttribute('side-width')).toBe(false)
    expect(sidebar.hasAttribute('content-min')).toBe(false)
    expect(sidebar.hasAttribute('gap')).toBe(false)
  })

  it('reflects set values (cols number reflects as a string attribute)', async () => {
    document.body.innerHTML =
      '<sema-container>a</sema-container><sema-grid cols="2"><div>b</div></sema-grid>' +
      '<sema-sidebar><div slot="aside">c</div><div>d</div></sema-sidebar>'
    const container = document.querySelector('sema-container')!
    const grid = document.querySelector('sema-grid')!
    const sidebar = document.querySelector('sema-sidebar')!
    expect(grid.cols).toBe(2)
    container.size = 'md'
    container.gutter = 'none'
    grid.min = '200px'
    grid.gap = 'xl'
    sidebar.sideWidth = '20rem'
    sidebar.contentMin = '60%'
    await Promise.all([container.updateComplete, grid.updateComplete, sidebar.updateComplete])
    expect(container.getAttribute('size')).toBe('md')
    expect(container.getAttribute('gutter')).toBe('none')
    expect(grid.getAttribute('min')).toBe('200px')
    expect(grid.getAttribute('cols')).toBe('2')
    expect(grid.getAttribute('gap')).toBe('xl')
    expect(sidebar.getAttribute('side-width')).toBe('20rem')
    expect(sidebar.getAttribute('content-min')).toBe('60%')
  })

  it('includes SemaElement.base (box-sizing: border-box on each host)', async () => {
    document.body.innerHTML =
      '<sema-container>a</sema-container><sema-grid><div>b</div></sema-grid>' +
      '<sema-sidebar><div slot="aside">c</div><div>d</div></sema-sidebar>'
    const container = document.querySelector('sema-container')!
    await container.updateComplete
    expect(getComputedStyle(container).boxSizing).toBe('border-box')
    expect(getComputedStyle(document.querySelector('sema-grid')!).boxSizing).toBe('border-box')
    expect(getComputedStyle(document.querySelector('sema-sidebar')!).boxSizing).toBe('border-box')
  })
})

describe('SemaContainer', () => {
  it('uses the lg max width by default and per-size values when set', async () => {
    document.body.innerHTML = `
      <sema-container>a</sema-container>
      <sema-container size="md">b</sema-container>
      <sema-container size="full">c</sema-container>`
    const [bare, md, full] = Array.from(document.querySelectorAll('sema-container'))
    await bare.updateComplete
    expect(getComputedStyle(bare).maxInlineSize).toBe('1200px')
    expect(getComputedStyle(md).maxInlineSize).toBe('1000px')
    expect(getComputedStyle(full).maxInlineSize).toBe('none')
  })

  it('centers itself with auto inline margins in a wide parent', async () => {
    document.body.innerHTML = `
      <div id="wrap" style="width: 1600px"><sema-container>x</sema-container></div>`
    const el = document.querySelector('sema-container')!
    await el.updateComplete
    const wrap = document.querySelector('#wrap')!.getBoundingClientRect()
    const rect = el.getBoundingClientRect()
    expect(rect.left - wrap.left).toBeCloseTo(200, 0)
    expect(wrap.right - rect.right).toBeCloseTo(200, 0)
  })

  it('defaults to the responsive clamp gutter for the test viewport', async () => {
    document.body.innerHTML = '<sema-container>x</sema-container>'
    const el = document.querySelector('sema-container')!
    await el.updateComplete
    const style = getComputedStyle(el)
    const pad = parseFloat(style.paddingLeft)
    // clamp(24px, 4vw, 32px) against the test viewport width
    const expected = Math.max(24, Math.min(window.innerWidth * 0.04, 32))
    expect(pad).toBeGreaterThanOrEqual(24)
    expect(pad).toBeLessThanOrEqual(32)
    expect(pad).toBeCloseTo(expected, 0)
    expect(style.paddingRight).toBe(style.paddingLeft)
  })

  it('maps gutter tokens onto inline padding (none = full bleed)', async () => {
    document.body.innerHTML = `
      <sema-container gutter="none">a</sema-container>
      <sema-container gutter="2xl">b</sema-container>`
    const [none, twoXl] = Array.from(document.querySelectorAll('sema-container'))
    await none.updateComplete
    expect(getComputedStyle(none).paddingLeft).toBe('0px')
    expect(getComputedStyle(twoXl).paddingLeft).toBe('48px')
  })

  it('lets an inherited --sema-container-max beat the token default, and an explicit size beat both', async () => {
    document.body.innerHTML = `
      <div style="--sema-container-max: 500px">
        <sema-container>a</sema-container>
        <sema-container size="md">b</sema-container>
      </div>`
    const [inherited, sized] = Array.from(document.querySelectorAll('sema-container'))
    await inherited.updateComplete
    expect(getComputedStyle(inherited).maxInlineSize).toBe('500px')
    expect(getComputedStyle(sized).maxInlineSize).toBe('1000px')
  })

  it('accepts --sema-container-max on the element itself', async () => {
    document.body.innerHTML =
      '<sema-container style="--sema-container-max: 500px">a</sema-container>'
    const el = document.querySelector('sema-container')!
    await el.updateComplete
    expect(getComputedStyle(el).maxInlineSize).toBe('500px')
  })

  it('projects slotted children through the default slot', async () => {
    document.body.innerHTML = '<sema-container><p>hello</p></sema-container>'
    const el = document.querySelector('sema-container')!
    await el.updateComplete
    const slot = el.shadowRoot!.querySelector('slot')!
    expect(slot.assignedElements()).toHaveLength(1)
    expect(slot.assignedElements()[0]).toBe(el.querySelector('p'))
    expect(el.querySelector('p')!.offsetWidth).toBeGreaterThan(0)
  })
})

describe('SemaGrid', () => {
  it('renders a display:grid wrapper exposed as part="grid" and projects children', async () => {
    document.body.innerHTML = '<sema-grid><div>a</div><div>b</div></sema-grid>'
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    const grid = el.shadowRoot!.querySelector('[part="grid"]')!
    expect(getComputedStyle(grid).display).toBe('grid')
    const slot = grid.querySelector('slot')!
    expect(slot.assignedElements()).toHaveLength(2)
    expect(slot.assignedElements()[0]).toBe(el.querySelector('div'))
  })

  it('matches documented effective defaults with no attributes set (gap md, min 240px)', async () => {
    document.body.innerHTML = `
      <div style="width: 1024px"><sema-grid>
        <div>a</div><div>b</div><div>c</div><div>d</div><div>e</div><div>f</div>
      </sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    const grid = el.shadowRoot!.querySelector('[part="grid"]')!
    expect(getComputedStyle(grid).columnGap).toBe('16px')
    const gap = parseFloat(getComputedStyle(grid).columnGap)
    expect(tracks(el)).toHaveLength(autoFillCount(1024, 240, gap))
  })

  it('lays out auto-fill columns from min at 1024px and reflows when the wrapper resizes', async () => {
    document.body.innerHTML = `
      <div id="wrap" style="width: 1024px"><sema-grid min="240px">
        <div>a</div><div>b</div><div>c</div><div>d</div><div>e</div><div>f</div>
      </sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    const grid = el.shadowRoot!.querySelector('[part="grid"]')!
    const gap = parseFloat(getComputedStyle(grid).columnGap)
    // 4×240 + 3×16 = 1008 ≤ 1024 — derived so a gap-token change can't silently break it
    expect(tracks(el)).toHaveLength(autoFillCount(1024, 240, gap))
    const wrap = document.querySelector('#wrap') as HTMLElement
    wrap.style.width = '520px'
    await waitFor(() => tracks(el).length === autoFillCount(520, 240, gap))
    expect(tracks(el)).toHaveLength(autoFillCount(520, 240, gap))
  })

  it('updates tracks when the min property changes and restores the default when cleared', async () => {
    document.body.innerHTML = `
      <div style="width: 1024px"><sema-grid>
        <div>a</div><div>b</div><div>c</div><div>d</div><div>e</div><div>f</div>
        <div>g</div><div>h</div>
      </sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    const grid = el.shadowRoot!.querySelector('[part="grid"]')!
    const gap = parseFloat(getComputedStyle(grid).columnGap)
    el.min = '120px'
    await el.updateComplete
    expect(el.getAttribute('min')).toBe('120px')
    expect(tracks(el)).toHaveLength(autoFillCount(1024, 120, gap))
    el.min = undefined
    await el.updateComplete
    expect(el.hasAttribute('min')).toBe(false)
    expect(el.style.getPropertyValue('--_min')).toBe('')
    expect(tracks(el)).toHaveLength(autoFillCount(1024, 240, gap))
  })

  it('honors an inherited --sema-grid-min when min is unset; an explicit min beats it', async () => {
    document.body.innerHTML = `
      <div style="width: 1024px; --sema-grid-min: 300px"><sema-grid>
        <div>a</div><div>b</div><div>c</div><div>d</div><div>e</div><div>f</div>
      </sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    const grid = el.shadowRoot!.querySelector('[part="grid"]')!
    const gap = parseFloat(getComputedStyle(grid).columnGap)
    expect(tracks(el)).toHaveLength(autoFillCount(1024, 300, gap))
    el.min = '200px'
    await el.updateComplete
    expect(tracks(el)).toHaveLength(autoFillCount(1024, 200, gap))
  })

  it('cols="2" renders two equal tracks', async () => {
    document.body.innerHTML = `
      <div style="width: 800px"><sema-grid cols="2"><div>a</div><div>b</div></sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    await waitFor(() => tracks(el).length === 2)
    const cols = tracks(el)
    expect(cols[0]).toBeCloseTo(cols[1], 0)
  })

  it('cols="2" collapses to one track below the 700px container threshold', async () => {
    document.body.innerHTML = `
      <div id="wrap" style="width: 900px"><sema-grid cols="2"><div>a</div><div>b</div></sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    await waitFor(() => tracks(el).length === 2)
    const wrap = document.querySelector('#wrap') as HTMLElement
    wrap.style.width = '500px'
    await waitFor(() => tracks(el).length === 1)
    expect(tracks(el)).toHaveLength(1)
  })

  it('never overflows a box narrower than min (the min(min, 100%) clamp)', async () => {
    document.body.innerHTML = `
      <div style="width: 200px"><sema-grid><div>a</div><div>b</div></sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    expect(el.scrollWidth).toBeLessThanOrEqual(el.clientWidth)
    expect(tracks(el)[0]).toBeLessThanOrEqual(200)
  })

  it('zeroes the slotted minimum so long content cannot blow out tracks', async () => {
    document.body.innerHTML = '<sema-grid><div>a</div></sema-grid>'
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    expect(getComputedStyle(el.querySelector('div')!).minInlineSize).toBe('0px')
  })

  it('lets a light-DOM grid-column span occupy two tracks', async () => {
    document.body.innerHTML = `
      <div style="width: 800px"><sema-grid cols="2">
        <div id="span" style="grid-column: span 2">wide</div>
        <div id="single">a</div>
        <div>b</div>
      </sema-grid></div>`
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    const grid = el.shadowRoot!.querySelector('[part="grid"]')!
    const gap = parseFloat(getComputedStyle(grid).columnGap)
    const span = document.querySelector('#span')!.getBoundingClientRect()
    const single = document.querySelector('#single')!.getBoundingClientRect()
    expect(span.width).toBeCloseTo(2 * single.width + gap, 0)
  })

  it('maps gap tokens (gap="xl" → 32px)', async () => {
    document.body.innerHTML = '<sema-grid gap="xl"><div>a</div></sema-grid>'
    const el = document.querySelector('sema-grid')!
    await el.updateComplete
    const grid = el.shadowRoot!.querySelector('[part="grid"]')!
    expect(getComputedStyle(grid).columnGap).toBe('32px')
    expect(getComputedStyle(grid).rowGap).toBe('32px')
  })
})

describe('SemaSidebar', () => {
  it('is a wrapping flex row with the md gap by default', async () => {
    document.body.innerHTML =
      '<sema-sidebar><nav slot="aside">n</nav><article>c</article></sema-sidebar>'
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    const style = getComputedStyle(el)
    expect(style.display).toBe('flex')
    expect(style.flexWrap).toBe('wrap')
    expect(style.columnGap).toBe('16px')
  })

  it('matches documented effective defaults with no attributes set (side 18rem, content-min 50%)', async () => {
    document.body.innerHTML =
      '<sema-sidebar><div slot="aside">a</div><div>c</div></sema-sidebar>'
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    expect(getComputedStyle(el.querySelector('[slot="aside"]')!).flexBasis).toBe('288px')
    expect(getComputedStyle(el.querySelector('div:not([slot])')!).minInlineSize).toBe('50%')
  })

  it('gives the aside its side-width basis and the content pane the growing share', async () => {
    document.body.innerHTML = `
      <div style="width: 900px">
        <sema-sidebar side-width="16rem" content-min="60%">
          <nav slot="aside">nav</nav>
          <article>content</article>
        </sema-sidebar>
      </div>`
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    const aside = getComputedStyle(el.querySelector('nav')!)
    const content = getComputedStyle(el.querySelector('article')!)
    expect(aside.flexBasis).toBe('256px')
    expect(aside.flexGrow).toBe('1')
    expect(content.flexGrow).toBe('999')
    expect(content.minInlineSize).toBe('60%')
  })

  it('keeps panes on one row in a wide parent and stacks them in a narrow one', async () => {
    document.body.innerHTML = `
      <div id="wrap" style="width: 900px">
        <sema-sidebar><div slot="aside">a</div><div>c</div></sema-sidebar>
      </div>`
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    const aside = el.querySelector('[slot="aside"]')!
    const content = el.querySelector('div:not([slot])')!
    expect(aside.getBoundingClientRect().top).toBeCloseTo(content.getBoundingClientRect().top, 0)
    const wrap = document.querySelector('#wrap') as HTMLElement
    wrap.style.width = '300px'
    await waitFor(() => aside.getBoundingClientRect().top !== content.getBoundingClientRect().top)
    expect(content.getBoundingClientRect().top).toBeGreaterThan(aside.getBoundingClientRect().top)
    expect(aside.getBoundingClientRect().width).toBeCloseTo(300, 0)
    expect(content.getBoundingClientRect().width).toBeCloseTo(300, 0)
  })

  it('uses an inherited --sema-sidebar-side when side-width is unset', async () => {
    document.body.innerHTML = `
      <sema-sidebar style="--sema-sidebar-side: 22rem">
        <div slot="aside">a</div><div>c</div>
      </sema-sidebar>`
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    expect(getComputedStyle(el.querySelector('[slot="aside"]')!).flexBasis).toBe('352px')
  })

  it('gap="none" removes the pane gap', async () => {
    document.body.innerHTML =
      '<sema-sidebar gap="none"><div slot="aside">a</div><div>c</div></sema-sidebar>'
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    expect(getComputedStyle(el).columnGap).toBe('0px')
    expect(getComputedStyle(el).rowGap).toBe('0px')
  })

  it('lets page CSS set align-items on the host (no shadow !important)', async () => {
    document.body.innerHTML = `
      <sema-sidebar style="align-items: flex-start">
        <div slot="aside">a</div><div>c</div>
      </sema-sidebar>`
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    expect(getComputedStyle(el).alignItems).toBe('flex-start')
  })

  it('assigns panes by slot, not source position', async () => {
    document.body.innerHTML = `
      <div style="width: 900px">
        <sema-sidebar>
          <article>content first in source</article>
          <nav slot="aside">aside last in source</nav>
        </sema-sidebar>
      </div>`
    const el = document.querySelector('sema-sidebar')!
    await el.updateComplete
    const nav = el.querySelector('nav')!
    const article = el.querySelector('article')!
    const asideSlot = el.shadowRoot!.querySelector('slot[name="aside"]') as HTMLSlotElement
    const defaultSlot = el.shadowRoot!.querySelector('slot:not([name])') as HTMLSlotElement
    expect(asideSlot.assignedElements()[0]).toBe(nav)
    expect(defaultSlot.assignedElements()[0]).toBe(article)
    // The aside pane renders before the content pane regardless of source order.
    expect(nav.getBoundingClientRect().left).toBeLessThan(article.getBoundingClientRect().left)
  })
})
