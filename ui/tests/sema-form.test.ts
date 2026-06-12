import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-input.js'
import '../src/lib/sema-textarea.js'
import '../src/lib/sema-select.js'
import '../src/lib/sema-field.js'
import type { SemaInput } from '../src/lib/sema-input.js'
import type { SemaSelect } from '../src/lib/sema-select.js'
import type { SemaTextarea } from '../src/lib/sema-textarea.js'

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

describe('SemaInput', () => {
  it('reflects typed value; native input crosses the shadow boundary exactly once', async () => {
    document.body.innerHTML = `<sema-input></sema-input>`
    const el = document.querySelector('sema-input') as SemaInput
    await el.updateComplete
    let emitted = 0
    el.addEventListener('input', () => emitted++)
    const input = el.shadowRoot!.querySelector('input')!
    input.value = 'hey'
    input.dispatchEvent(new Event('input', { bubbles: true, composed: true }))
    expect(el.value).toBe('hey')
    expect(emitted).toBe(1)
  })

  it('participates in a form (FormData picks up the value by name)', async () => {
    document.body.innerHTML = `<form><sema-input name="pkg"></sema-input></form>`
    const el = document.querySelector('sema-input') as SemaInput
    el.value = 'http-helpers'
    await el.updateComplete
    const fd = new FormData(document.querySelector('form')!)
    expect(fd.get('pkg')).toBe('http-helpers')
  })

  it('resets with the form', async () => {
    document.body.innerHTML = `<form><sema-input name="x"></sema-input></form>`
    const el = document.querySelector('sema-input') as SemaInput
    el.value = 'temp'
    await el.updateComplete
    document.querySelector('form')!.reset()
    await el.updateComplete
    expect(el.value).toBe('')
  })
})

describe('SemaTextarea', () => {
  it('uses the shared themed scrollbar class', async () => {
    document.body.innerHTML = `<sema-textarea></sema-textarea>`
    const el = document.querySelector('sema-textarea')!
    await (el as unknown as { updateComplete: Promise<unknown> }).updateComplete
    expect(el.shadowRoot!.querySelector('textarea')!.classList.contains('sema-scroll')).toBe(true)
  })

  it('reflects the autosize attribute', async () => {
    document.body.innerHTML = `<sema-textarea autosize></sema-textarea>`
    const el = document.querySelector('sema-textarea')!
    await (el as unknown as { updateComplete: Promise<unknown> }).updateComplete
    expect(el.hasAttribute('autosize')).toBe(true)
  })

  it('autosize grows the textarea as content grows', async () => {
    document.body.innerHTML = `<sema-textarea autosize></sema-textarea>`
    const el = document.querySelector('sema-textarea') as SemaTextarea
    await el.updateComplete
    const ta = el.shadowRoot!.querySelector('textarea')!
    // Where supported, growth comes from CSS field-sizing; otherwise the
    // scrollHeight fallback sets an inline height. Both must grow the box.
    if (CSS.supports('field-sizing', 'content')) {
      expect(getComputedStyle(ta).getPropertyValue('field-sizing')).toBe('content')
    }
    const before = ta.getBoundingClientRect().height
    el.value = Array.from({ length: 10 }, (_, i) => `line ${i + 1}`).join('\n')
    await el.updateComplete
    expect(ta.getBoundingClientRect().height).toBeGreaterThan(before)
  })
})

describe('SemaSelect (custom dropdown)', () => {
  const OPTIONS = `<option value="tw">Tree-walker</option><option value="vm">Bytecode VM</option>`

  it('renders a listbox of options and reflects the selected value', async () => {
    document.body.innerHTML = `<sema-select value="vm">${OPTIONS}</sema-select>`
    const el = document.querySelector('sema-select') as SemaSelect
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelectorAll('[role="option"]').length === 2)
    const opts = el.shadowRoot!.querySelectorAll('[role="option"]')
    expect(opts.length).toBe(2)
    expect(el.shadowRoot!.querySelector('.trigger .label')!.textContent).toContain('Bytecode VM')
    expect(opts[1].getAttribute('aria-selected')).toBe('true')
    expect(el.shadowRoot!.querySelector('select')).toBeNull() // not native
  })

  it('updates value + emits change when an option is clicked', async () => {
    document.body.innerHTML = `<sema-select value="tw">${OPTIONS}</sema-select>`
    const el = document.querySelector('sema-select') as SemaSelect
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelectorAll('[role="option"]').length === 2)
    let changed = 0
    el.addEventListener('change', () => changed++)
    ;(el.shadowRoot!.querySelectorAll('[role="option"]')[1] as HTMLButtonElement).click()
    await el.updateComplete
    expect(el.value).toBe('vm')
    expect(changed).toBe(1)
  })

  it('falls back to a native <select> with the `native` flag', async () => {
    document.body.innerHTML = `<sema-select native value="vm">${OPTIONS}</sema-select>`
    const el = document.querySelector('sema-select') as SemaSelect
    await el.updateComplete
    const select = el.shadowRoot!.querySelector('select')!
    await waitFor(() => select.querySelectorAll('option').length === 2)
    expect(select).toBeTruthy()
    expect(select.value).toBe('vm')
    expect(el.shadowRoot!.querySelector('[role="option"]')).toBeNull() // not custom
  })
})

describe('SemaSelect — keyboard (custom dropdown)', () => {
  const OPTIONS = `<option value="tw">Tree-walker</option><option value="vm">Bytecode VM</option><option value="jit">JIT</option>`

  async function setup(value: string): Promise<SemaSelect> {
    document.body.innerHTML = `<sema-select value="${value}">${OPTIONS}</sema-select>`
    const el = document.querySelector('sema-select') as SemaSelect
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelectorAll('[role="option"]').length === 3)
    return el
  }

  function trigger(el: SemaSelect): HTMLButtonElement {
    return el.shadowRoot!.querySelector('.trigger') as HTMLButtonElement
  }

  async function open(el: SemaSelect): Promise<void> {
    trigger(el).dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown' }))
    await el.updateComplete
    // Focus is deferred via sema-select's own requestAnimationFrame (queued before
    // this one, so awaiting a single frame is enough).
    await new Promise((r) => requestAnimationFrame(r))
  }

  function activeOption(el: SemaSelect): string | undefined {
    return (el.shadowRoot!.activeElement as HTMLElement | null)?.dataset?.value
  }

  // The listbox div's @keydown handler does the roving; the options live in the same
  // shadow root, so a bubbling dispatch from the focused option reaches it.
  function press(el: SemaSelect, key: string) {
    ;(el.shadowRoot!.activeElement as HTMLElement).dispatchEvent(
      new KeyboardEvent('keydown', { key, bubbles: true, composed: true }),
    )
  }

  it('ArrowDown on the trigger opens the listbox and focuses the selected option', async () => {
    const el = await setup('vm')
    expect(trigger(el).getAttribute('aria-expanded')).toBe('false')
    await open(el)
    expect(trigger(el).getAttribute('aria-expanded')).toBe('true')
    expect(activeOption(el)).toBe('vm')
  })

  it('arrow keys rove and wrap focus in the listbox; Home/End jump', async () => {
    const el = await setup('tw')
    await open(el)
    expect(activeOption(el)).toBe('tw')
    press(el, 'ArrowDown')
    expect(activeOption(el)).toBe('vm')
    press(el, 'ArrowDown')
    expect(activeOption(el)).toBe('jit')
    press(el, 'ArrowDown')
    expect(activeOption(el)).toBe('tw') // wraps forward
    press(el, 'ArrowUp')
    expect(activeOption(el)).toBe('jit') // wraps backward
    press(el, 'Home')
    expect(activeOption(el)).toBe('tw')
    press(el, 'End')
    expect(activeOption(el)).toBe('jit')
  })

  it('Enter selects the focused option, emits change, and closes the listbox', async () => {
    const el = await setup('tw')
    let changed = 0
    el.addEventListener('change', () => changed++)
    await open(el)
    press(el, 'ArrowDown') // vm
    press(el, 'Enter')
    await el.updateComplete
    expect(el.value).toBe('vm')
    expect(changed).toBe(1)
    expect(trigger(el).getAttribute('aria-expanded')).toBe('false')
  })
})

describe('SemaField', () => {
  it('renders label and hint; error replaces hint', async () => {
    document.body.innerHTML = `<sema-field label="Email" hint="be careful"><input></sema-field>`
    const el = document.querySelector('sema-field')!
    await (el as unknown as { updateComplete: Promise<unknown> }).updateComplete
    expect(el.shadowRoot!.querySelector('.label')!.textContent).toBe('Email')
    expect(el.shadowRoot!.querySelector('.msg')!.textContent).toContain('be careful')

    el.setAttribute('error', 'required')
    await (el as unknown as { updateComplete: Promise<unknown> }).updateComplete
    const msg = el.shadowRoot!.querySelector('.msg')!
    expect(msg.classList.contains('error')).toBe(true)
    expect(msg.textContent).toContain('required')
  })
})
