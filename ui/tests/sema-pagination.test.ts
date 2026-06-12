import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-pagination.js'
import { paginationItems } from '../src/lib/sema-pagination.js'
import type { SemaPagination } from '../src/lib/sema-pagination.js'
import type { SemaPageChangeEventDetail } from '../src/lib/events.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

function mount(attrs = ''): SemaPagination {
  document.body.innerHTML = `<sema-pagination ${attrs}></sema-pagination>`
  return document.querySelector('sema-pagination')!
}

function pageLabels(el: SemaPagination): string[] {
  return [...el.shadowRoot!.querySelectorAll('[part~="page"]')].map((b) => b.textContent!.trim())
}

describe('paginationItems (truncation)', () => {
  it('shows every page when there is no overflow', () => {
    expect(paginationItems(3, 5)).toEqual([1, 2, 3, 4, 5])
  })

  it('keeps a stable width near the start', () => {
    expect(paginationItems(1, 10)).toEqual([1, 2, 3, 4, 5, 'end-ellipsis', 10])
  })

  it('places ellipses on both sides in the middle', () => {
    expect(paginationItems(5, 10)).toEqual([1, 'start-ellipsis', 4, 5, 6, 'end-ellipsis', 10])
  })

  it('keeps a stable width near the end', () => {
    expect(paginationItems(10, 10)).toEqual([1, 'start-ellipsis', 6, 7, 8, 9, 10])
  })

  it('returns a single page for count 1 and nothing for count 0', () => {
    expect(paginationItems(1, 1)).toEqual([1])
    expect(paginationItems(1, 0)).toEqual([])
  })
})

describe('SemaPagination', () => {
  it('renders nothing when total < 1', async () => {
    const el = mount('total="0"')
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('nav')).toBeNull()
  })

  it('marks the current page with aria-current', async () => {
    const el = mount('page="3" total="10"')
    await el.updateComplete
    const current = el.shadowRoot!.querySelector('[aria-current="page"]')!
    expect(current.textContent!.trim()).toBe('3')
  })

  it('disables prev on the first page and next on the last', async () => {
    const first = mount('page="1" total="5"')
    await first.updateComplete
    expect((first.shadowRoot!.querySelector('[part~="prev"]') as HTMLButtonElement).disabled).toBe(true)
    expect((first.shadowRoot!.querySelector('[part~="next"]') as HTMLButtonElement).disabled).toBe(false)

    const last = mount('page="5" total="5"')
    await last.updateComplete
    expect((last.shadowRoot!.querySelector('[part~="prev"]') as HTMLButtonElement).disabled).toBe(false)
    expect((last.shadowRoot!.querySelector('[part~="next"]') as HTMLButtonElement).disabled).toBe(true)
  })

  it('fires sema-page-change with the new page on click', async () => {
    const el = mount('page="3" total="10"')
    await el.updateComplete
    let detail: SemaPageChangeEventDetail | undefined
    el.addEventListener('sema-page-change', (e) => (detail = (e as CustomEvent).detail))
    el.shadowRoot!.querySelector<HTMLButtonElement>('[part~="next"]')!.click()
    await el.updateComplete
    expect(detail).toEqual({ page: 4 })
    expect(el.page).toBe(4)
  })

  it('does not fire when clicking the current page', async () => {
    const el = mount('page="3" total="10"')
    await el.updateComplete
    let fired = 0
    el.addEventListener('sema-page-change', () => fired++)
    el.shadowRoot!.querySelector<HTMLButtonElement>('[part~="current"]')!.click()
    await el.updateComplete
    expect(fired).toBe(0)
  })

  it('clamps an out-of-range page to the last page', async () => {
    const el = mount('page="99" total="5"')
    await el.updateComplete
    const current = el.shadowRoot!.querySelector('[aria-current="page"]')!
    expect(current.textContent!.trim()).toBe('5')
    expect(pageLabels(el)).toContain('5')
  })
})
