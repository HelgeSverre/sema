import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-splitter.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaSplitter', () => {
  it('reflects direction attribute', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.getAttribute('direction')).toBe('horizontal')
  })

  it('defaults to horizontal direction', async () => {
    document.body.innerHTML = '<sema-splitter></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.direction).toBe('horizontal')
  })

  it('accepts direction="vertical"', async () => {
    document.body.innerHTML = '<sema-splitter direction="vertical"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.direction).toBe('vertical')
  })

  it('has role="separator"', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.getAttribute('role')).toBe('separator')
  })

  it('is focusable', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.tabIndex).toBe(0)
  })

  it('has configurable step and shiftStep', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal" step="20" shiftStep="100"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.step).toBe(20)
    expect(el.shiftStep).toBe(100)
  })

  it('has default min and max values', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.min).toBe(0)
    expect(el.max).toBe(Infinity)
  })

  it('dispatches sema-resize-start on mousedown', async () => {
    document.body.innerHTML = `
      <div style="display:flex;width:400px;height:80px;">
        <div style="width:200px">A</div>
        <sema-splitter id="s" direction="horizontal"></sema-splitter>
        <div style="flex:1">B</div>
      </div>`
    const el = document.querySelector('#s')!
    let started = false
    el.addEventListener('sema-resize-start', () => { started = true })

    const rect = el.getBoundingClientRect()
    el.dispatchEvent(new MouseEvent('mousedown', {
      clientX: rect.left, clientY: rect.top, bubbles: true
    }))
    expect(started).toBe(true)
  })

  it('uses configurable min and max', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal" min="100" max="500"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    expect(el.min).toBe(100)
    expect(el.max).toBe(500)
  })

  it('maps direction to the separator\'s own aria-orientation', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    await el.updateComplete
    // A col-resize (horizontal layout) splitter is itself a vertical bar.
    expect(el.getAttribute('aria-orientation')).toBe('vertical')
    el.direction = 'vertical'
    await el.updateComplete
    expect(el.getAttribute('aria-orientation')).toBe('horizontal')
  })

  it('exposes aria-valuemin and omits aria-valuemax while max is Infinity', async () => {
    document.body.innerHTML = '<sema-splitter min="100"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    await el.updateComplete
    expect(el.getAttribute('aria-valuemin')).toBe('100')
    expect(el.hasAttribute('aria-valuemax')).toBe(false)
  })

  it('reflects runtime min/max changes to ARIA attributes', async () => {
    document.body.innerHTML = '<sema-splitter></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    await el.updateComplete
    el.min = 50
    el.max = 500
    await el.updateComplete
    expect(el.getAttribute('aria-valuemin')).toBe('50')
    expect(el.getAttribute('aria-valuemax')).toBe('500')
    el.max = Infinity
    await el.updateComplete
    expect(el.hasAttribute('aria-valuemax')).toBe(false)
  })

  it('setValue sets aria-valuenow and optional aria-valuetext', async () => {
    document.body.innerHTML = '<sema-splitter></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    el.setValue(240)
    expect(el.getAttribute('aria-valuenow')).toBe('240')
    el.setValue(260, '260 pixels')
    expect(el.getAttribute('aria-valuenow')).toBe('260')
    expect(el.getAttribute('aria-valuetext')).toBe('260 pixels')
    el.setValue(280)
    expect(el.hasAttribute('aria-valuetext')).toBe(false)
  })

  it('tears down drag state on mid-drag disconnect without firing sema-resize-end', async () => {
    document.body.innerHTML = '<sema-splitter id="s" direction="horizontal"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    let ended = false
    el.addEventListener('sema-resize-end', () => { ended = true })
    el.dispatchEvent(new MouseEvent('mousedown', { clientX: 10, clientY: 10, bubbles: true }))
    expect(document.body.style.cursor).toBe('col-resize')
    expect(el.hasAttribute('dragging')).toBe(true)
    el.remove()
    expect(document.body.style.cursor).toBe('')
    expect(document.body.style.userSelect).toBe('')
    expect(el.hasAttribute('dragging')).toBe(false)
    expect(ended).toBe(false)
  })

  it('dispatches sema-resize-end on mouseup while connected', async () => {
    document.body.innerHTML = '<sema-splitter direction="horizontal"></sema-splitter>'
    const el = document.querySelector('sema-splitter')!
    let ended = false
    el.addEventListener('sema-resize-end', () => { ended = true })
    el.dispatchEvent(new MouseEvent('mousedown', { clientX: 10, clientY: 10, bubbles: true }))
    document.dispatchEvent(new MouseEvent('mouseup'))
    expect(ended).toBe(true)
    expect(document.body.style.cursor).toBe('')
  })
})
