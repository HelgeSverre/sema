import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-code-typer.js'
import type { SemaCodeTyper } from '../src/lib/sema-code-typer.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

async function waitFor(fn: () => unknown, timeout = 3000): Promise<void> {
  const start = performance.now()
  while (performance.now() - start < timeout) {
    if (fn()) return
    await new Promise((r) => setTimeout(r, 20))
  }
  throw new Error('waitFor timed out')
}

const stripNl = (s: string) => s.replace(/\n/g, '')
function codeText(el: SemaCodeTyper): string {
  return el.shadowRoot!.querySelector('.code')?.textContent ?? ''
}

async function mount(code: string, init?: (el: SemaCodeTyper) => void): Promise<SemaCodeTyper> {
  const el = document.createElement('sema-code-typer') as SemaCodeTyper
  el.autoplay = false // deterministic: reveal via seek(), no rAF timing
  init?.(el)
  el.textContent = code
  document.body.appendChild(el)
  await el.updateComplete
  // wait until tokenization resolved (seek clamps to total, which is 0 until then)
  await waitFor(() => {
    el.seek(code.length)
    return codeText(el).length === stripNl(code).length && codeText(el).length > 0
  })
  return el
}

describe('SemaCodeTyper', () => {
  it('seek(n) reveals exactly the first n characters', async () => {
    const code = '(define (square x) (* x x))'
    const el = await mount(code)
    el.seek(7)
    await el.updateComplete
    expect(codeText(el)).toBe('(define')
    el.seek(0)
    await el.updateComplete
    expect(codeText(el)).toBe('')
  })

  it('reveal stays correct across newlines', async () => {
    const code = '(define x 1)\n(define y 2)'
    const el = await mount(code)
    el.seek(16) // through "(define x 1)\n(de"
    await el.updateComplete
    expect(codeText(el)).toBe(stripNl(code.slice(0, 16)))
  })

  it('syntax-highlights the revealed code (tok-* spans)', async () => {
    const el = await mount('(define x 1)')
    el.seek(12)
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('[class^="tok-"]')).toBeTruthy()
  })

  it('renders a caret', async () => {
    const el = await mount('(+ 1 2)')
    el.seek(3)
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('.cursor')).toBeTruthy()
  })

  it('frame chrome renders the legend and status with Ln:Col', async () => {
    const el = await mount('(define x 1)', (e) => {
      e.frame = true
      e.status = true
      e.filename = 'demo.sema'
    })
    el.seek(8)
    await el.updateComplete
    expect(el.shadowRoot!.querySelector('.frame')).toBeTruthy()
    expect(el.shadowRoot!.querySelector('.legend .lname')!.textContent).toBe('sema')
    const status = el.shadowRoot!.querySelector('.status')!
    expect(status.querySelector('.mode')!.textContent).toBe('EDIT')
    expect(status.textContent).toContain('demo.sema')
    expect(status.querySelector('.pos')!.textContent).toBe('1:9')
  })

  it('bare (no frame) renders just the editor, no frame/status', async () => {
    const el = await mount('(+ 1 2)')
    expect(el.shadowRoot!.querySelector('.frame')).toBeNull()
    expect(el.shadowRoot!.querySelector('.status')).toBeNull()
    expect(el.shadowRoot!.querySelector('.code')).toBeTruthy()
  })

  it('autoplay types to completion and fires sema-typer-done', async () => {
    const el = document.createElement('sema-code-typer') as SemaCodeTyper
    el.cps = 5000
    el.startDelay = 0
    let done = false
    el.addEventListener('sema-typer-done', () => (done = true))
    el.textContent = '(define (id x) x)'
    document.body.appendChild(el)
    await waitFor(() => done)
    expect(done).toBe(true)
    expect(codeText(el)).toBe(stripNl('(define (id x) x)'))
  })
})
