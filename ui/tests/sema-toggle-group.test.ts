import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-toggle.js'
import '../src/lib/sema-toggle-group.js'
import type { SemaToggle } from '../src/lib/sema-toggle.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

// The group's keydown listener lives on its shadow .group div; dispatch from the
// toggle's inner .toggle div so the composed event crosses the slot like a real keypress.
function press(toggle: SemaToggle, key: string) {
  const inner = toggle.shadowRoot!.querySelector('.toggle') as HTMLElement
  inner.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, composed: true }))
}

describe('SemaToggleGroup', () => {
  it('sets initial selected toggle based on value', async () => {
    document.body.innerHTML = `
      <sema-toggle-group value="vm">
        <sema-toggle value="tw">Tree-walker</sema-toggle>
        <sema-toggle value="vm">Bytecode VM</sema-toggle>
      </sema-toggle-group>`
    const group = document.querySelector('sema-toggle-group')!
    await group.updateComplete
    const toggles = document.querySelectorAll('sema-toggle')
    for (const t of toggles) await t.updateComplete
    expect(toggles[0].selected).toBe(false)
    expect(toggles[1].selected).toBe(true)
  })

  it('selects toggle on click', async () => {
    document.body.innerHTML = `
      <sema-toggle-group value="tw">
        <sema-toggle value="tw">Tree-walker</sema-toggle>
        <sema-toggle value="vm">Bytecode VM</sema-toggle>
      </sema-toggle-group>`
    const group = document.querySelector('sema-toggle-group')!
    const toggles = document.querySelectorAll('sema-toggle')
    for (const t of toggles) await t.updateComplete
    await group.updateComplete

    const target = toggles[1].shadowRoot!.querySelector('.toggle') as HTMLElement
    target.click()
    await group.updateComplete

    expect(toggles[0].selected).toBe(false)
    expect(toggles[1].selected).toBe(true)
    expect(group.value).toBe('vm')
  })

  it('dispatches sema-change event on selection', async () => {
    document.body.innerHTML = `
      <sema-toggle-group value="tw">
        <sema-toggle value="tw">Tree-walker</sema-toggle>
        <sema-toggle value="vm">Bytecode VM</sema-toggle>
      </sema-toggle-group>`
    const group = document.querySelector('sema-toggle-group')!
    const toggles = document.querySelectorAll('sema-toggle')
    for (const t of toggles) await t.updateComplete
    await group.updateComplete

    let changed = false
    let detail: { value: string } | null = null
    group.addEventListener('sema-change', (e) => {
      changed = true
      detail = (e as CustomEvent).detail
    })

    const target = toggles[1].shadowRoot!.querySelector('.toggle') as HTMLElement
    target.click()

    expect(changed).toBe(true)
    expect(detail).toEqual({ value: 'vm' })
  })

  it('renders with role radiogroup', async () => {
    document.body.innerHTML = `
      <sema-toggle-group value="tw">
        <sema-toggle value="tw">A</sema-toggle>
        <sema-toggle value="vm">B</sema-toggle>
      </sema-toggle-group>`
    const group = document.querySelector('sema-toggle-group')!
    await group.updateComplete
    const el = group.shadowRoot!.querySelector('.group')!
    expect(el.getAttribute('role')).toBe('radiogroup')
  })

  it('arrow keys rove focus through the toggles and wrap', async () => {
    document.body.innerHTML = `
      <sema-toggle-group value="tw">
        <sema-toggle value="tw">Tree-walker</sema-toggle>
        <sema-toggle value="vm">Bytecode VM</sema-toggle>
        <sema-toggle value="jit">JIT</sema-toggle>
      </sema-toggle-group>`
    const group = document.querySelector('sema-toggle-group')!
    const toggles = document.querySelectorAll('sema-toggle')
    for (const t of toggles) await t.updateComplete
    await group.updateComplete

    // document.activeElement retargets shadow focus to the sema-toggle host
    toggles[0].focus()
    expect(document.activeElement).toBe(toggles[0])

    press(toggles[0], 'ArrowRight')
    expect(document.activeElement).toBe(toggles[1])
    expect(toggles[1].tabbable).toBe(true)
    expect(toggles[0].tabbable).toBe(false)

    press(toggles[1], 'ArrowDown') // ArrowDown behaves like ArrowRight
    expect(document.activeElement).toBe(toggles[2])

    press(toggles[2], 'ArrowRight') // wraps forward to the first
    expect(document.activeElement).toBe(toggles[0])

    press(toggles[0], 'ArrowLeft') // wraps backward to the last
    expect(document.activeElement).toBe(toggles[2])

    press(toggles[2], 'ArrowUp') // ArrowUp behaves like ArrowLeft
    expect(document.activeElement).toBe(toggles[1])
  })

  it('Space and Enter select the focused toggle and emit sema-change', async () => {
    document.body.innerHTML = `
      <sema-toggle-group value="tw">
        <sema-toggle value="tw">Tree-walker</sema-toggle>
        <sema-toggle value="vm">Bytecode VM</sema-toggle>
      </sema-toggle-group>`
    const group = document.querySelector('sema-toggle-group')!
    const toggles = document.querySelectorAll('sema-toggle')
    for (const t of toggles) await t.updateComplete
    await group.updateComplete

    const values: string[] = []
    group.addEventListener('sema-change', (e) => values.push((e as CustomEvent).detail.value))

    toggles[1].focus()
    press(toggles[1], ' ')
    await group.updateComplete
    expect(group.value).toBe('vm')
    expect(toggles[1].selected).toBe(true)
    expect(toggles[0].selected).toBe(false)

    toggles[0].focus()
    press(toggles[0], 'Enter')
    await group.updateComplete
    expect(group.value).toBe('tw')
    expect(toggles[0].selected).toBe(true)
    expect(toggles[1].selected).toBe(false)
    expect(values).toEqual(['vm', 'tw'])
  })
})
