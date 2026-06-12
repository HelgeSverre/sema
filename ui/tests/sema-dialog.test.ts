import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-dialog.js'
import '../src/lib/sema-button.js'
import type { SemaDialog } from '../src/lib/sema-dialog.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaDialog', () => {
  it('is not visible when open is false', async () => {
    document.body.innerHTML = '<sema-dialog id="d"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    expect(el.open).toBe(false)
    expect(el.shadowRoot!.querySelector('.backdrop')).toBeNull()
  })

  it('shows backdrop and dialog content when opened', async () => {
    document.body.innerHTML = '<sema-dialog id="d" open label="Test"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    expect(el.open).toBe(true)
    expect(el.shadowRoot!.querySelector('.backdrop')).toBeTruthy()
    expect(el.shadowRoot!.querySelector('.dialog')).toBeTruthy()
  })

  it('has role="dialog" and aria-modal', async () => {
    document.body.innerHTML = '<sema-dialog id="d" open label="Test"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    const dialog = el.shadowRoot!.querySelector('.dialog')!
    expect(dialog.getAttribute('role')).toBe('dialog')
    expect(dialog.getAttribute('aria-modal')).toBe('true')
  })

  it('renders label in header', async () => {
    document.body.innerHTML = '<sema-dialog id="d" open label="Confirm"><p>Body</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    const header = el.shadowRoot!.querySelector('.header')
    expect(header).toBeTruthy()
    expect(header!.textContent).toContain('Confirm')
  })

  it('renders slotted footer content', async () => {
    document.body.innerHTML = `
      <sema-dialog id="d" open label="Test">
        <p>Body</p>
        <div slot="footer"><button>OK</button></div>
      </sema-dialog>`
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    const footerSlot = el.shadowRoot!.querySelector('.footer slot') as HTMLSlotElement
    expect(footerSlot).toBeTruthy()
    expect(footerSlot.assignedElements().length).toBeGreaterThan(0)
  })

  it('can be opened via show() method', async () => {
    document.body.innerHTML = '<sema-dialog id="d" label="Test"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    expect(el.open).toBe(false)
    el.show()
    await el.updateComplete
    expect(el.open).toBe(true)
    el.close()
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('locks body scroll when open and unlocks on close', async () => {
    document.body.innerHTML = '<sema-dialog id="d" label="Test"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    el.show()
    await el.updateComplete
    expect(document.body.style.overflow).toBe('hidden')
    el.close()
    await el.updateComplete
    expect(document.body.style.overflow).toBe('')
  })

  it('Escape closes the dialog wherever focus is (document-level listener)', async () => {
    document.body.innerHTML = '<sema-dialog id="d" open label="Test"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    // Dispatched from body — outside the dialog's shadow root — to prove the
    // listener lives on document, not inside the shadow tree.
    document.body.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true, composed: true }))
    await el.updateComplete
    expect(el.open).toBe(false)
    expect(el.shadowRoot!.querySelector('.backdrop')).toBeNull()
  })

  it('backdrop click closes the dialog', async () => {
    document.body.innerHTML = '<sema-dialog id="d" open label="Test"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    ;(el.shadowRoot!.querySelector('.backdrop') as HTMLElement).click()
    await el.updateComplete
    expect(el.open).toBe(false)
  })

  it('clicks inside the dialog do not close it', async () => {
    document.body.innerHTML = '<sema-dialog id="d" open label="Test"><p>Content</p></sema-dialog>'
    const el = document.querySelector('#d') as SemaDialog
    await el.updateComplete
    ;(el.shadowRoot!.querySelector('.dialog') as HTMLElement).click()
    ;(el.querySelector('p') as HTMLElement).click() // slotted content
    await el.updateComplete
    expect(el.open).toBe(true)
  })
})

describe('SemaDialog — focus trap (e2e)', () => {
  function innerButton(el: HTMLElement, index: number): HTMLButtonElement {
    const buttons = el.querySelectorAll('sema-button')
    return (buttons[index]?.shadowRoot?.querySelector('button') as HTMLButtonElement) ?? null!
  }

  function setupDialog(): SemaDialog {
    document.body.innerHTML = `
      <sema-dialog id="ft-dialog" label="Focus Trap Test">
        <p>Body text</p>
        <div slot="footer">
          <sema-button variant="ghost" id="ft-cancel">Cancel</sema-button>
          <sema-button variant="primary" id="ft-confirm">Confirm</sema-button>
        </div>
      </sema-dialog>`
    return document.querySelector('#ft-dialog')!
  }

  it('focuses first sema-button when dialog opens', async () => {
    const dialog = setupDialog()
    await dialog.updateComplete

    dialog.show()
    await dialog.updateComplete
    // Focus is deferred via requestAnimationFrame — wait one frame
    await new Promise((r) => requestAnimationFrame(r))

    // With delegatesFocus on sema-button, document.activeElement reports
    // the host, and the inner <button> also matches :focus
    expect(document.activeElement).toBe(dialog.querySelector('#ft-cancel'))
  })

  it('Tab from last button wraps to first', async () => {
    const dialog = setupDialog()
    await dialog.updateComplete
    dialog.show()
    await dialog.updateComplete

    const cancelHost = dialog.querySelector('#ft-cancel')!

    // Manually focus confirm (last button)
    const confirmInner = innerButton(dialog, 1)
    confirmInner.focus()

    // Dispatch Tab on the focused inner button — event bubbles up through
    // sema-button shadow → sema-button host → dialog light DOM → sema-dialog host
    // The controller's listener on the host catches it
    confirmInner.dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Tab', bubbles: true, composed: true }),
    )

    // Should have wrapped back to Cancel
    expect(document.activeElement).toBe(cancelHost)
  })

  it('Shift+Tab from first button wraps to last', async () => {
    const dialog = setupDialog()
    await dialog.updateComplete
    dialog.show()
    await dialog.updateComplete

    const confirmHost = dialog.querySelector('#ft-confirm')!

    const cancelInner = innerButton(dialog, 0)
    cancelInner.focus()

    cancelInner.dispatchEvent(
      new KeyboardEvent('keydown', {
        key: 'Tab',
        shiftKey: true,
        bubbles: true,
        composed: true,
      }),
    )

    expect(document.activeElement).toBe(confirmHost)
  })

  it('focuses first sema-button when created with open attribute', async () => {
    document.body.innerHTML = `
      <sema-dialog id="ft-open" label="Open Test" open>
        <p>Body</p>
        <div slot="footer">
          <sema-button variant="ghost">Cancel</sema-button>
          <sema-button variant="primary">Confirm</sema-button>
        </div>
      </sema-dialog>`
    const dialog = document.querySelector('#ft-open') as SemaDialog
    await dialog.updateComplete
    // Focus is deferred via requestAnimationFrame — wait one frame
    await new Promise((r) => requestAnimationFrame(r))

    expect(document.activeElement).toBe(dialog.querySelector('sema-button'))
  })

  it('returns focus to trigger when dialog closes', async () => {
    document.body.innerHTML = `
      <button id="trigger">Open</button>
      <sema-dialog id="ft-close" label="Close Test">
        <p>Body</p>
        <div slot="footer">
          <sema-button variant="primary">Close</sema-button>
        </div>
      </sema-dialog>`
    const trigger = document.querySelector('#trigger') as HTMLElement
    const dialog = document.querySelector('#ft-close') as SemaDialog

    trigger.focus()
    await dialog.updateComplete

    dialog.show()
    await dialog.updateComplete
    expect(document.activeElement).not.toBe(trigger)

    dialog.close()
    await dialog.updateComplete

    expect(document.activeElement).toBe(trigger)
  })
})
