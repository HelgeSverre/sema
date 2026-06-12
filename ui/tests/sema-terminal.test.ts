import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-terminal.js'
import type { SemaTerminal } from '../src/lib/sema-terminal.js'

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

function term(): SemaTerminal {
  return document.querySelector('sema-terminal') as SemaTerminal
}
function lines(el: SemaTerminal) {
  return Array.from(el.shadowRoot!.querySelectorAll('.term-line'))
}
function q(el: SemaTerminal, sel: string) {
  return el.shadowRoot!.querySelector(sel)
}
function qa(el: SemaTerminal, sel: string) {
  return el.shadowRoot!.querySelectorAll(sel)
}

describe('SemaTerminal', () => {
  it('renders a "$ " line as a command with a prompt glyph', async () => {
    document.body.innerHTML = `<sema-terminal>$ brew install sema-lang</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => lines(el).length >= 1)
    expect(q(el, '.term-prompt')!.textContent).toBe('$')
    expect(lines(el)[0].textContent).toBe('$ brew install sema-lang')
  })

  it('renders a non-prompt line as dim output', async () => {
    document.body.innerHTML = `<sema-terminal>$ sema run\nHello, world!</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => lines(el).length >= 2)
    const out = q(el, '.term-output')!
    expect(out.textContent).toBe('Hello, world!')
    // output lines carry no prompt
    expect(qa(el, '.term-prompt').length).toBe(1)
  })

  it('splits a trailing "# " comment into tok-comment', async () => {
    document.body.innerHTML = `<sema-terminal>$ sema script.sema  # Run a file</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => q(el, '.tok-comment'))
    expect(q(el, '.tok-comment')!.textContent).toBe('# Run a file')
  })

  it('does NOT treat #t inside an arg as a comment', async () => {
    document.body.innerHTML = `<sema-terminal>$ sema -e '(if #t 1 2)'</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => lines(el).length >= 1)
    expect(q(el, '.tok-comment')).toBeNull()
    expect(lines(el)[0].textContent).toBe(`$ sema -e '(if #t 1 2)'`)
  })

  it('colors quoted args as strings', async () => {
    document.body.innerHTML = `<sema-terminal>$ sema -e '(+ 1 2)'</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => q(el, '.tok-string'))
    expect(q(el, '.tok-string')!.textContent).toBe(`'(+ 1 2)'`)
  })

  it('prefix mode prepends the prompt to every line', async () => {
    document.body.innerHTML = `<sema-terminal prefix>sema compile app.sema\nsema fmt --check</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => lines(el).length >= 2)
    expect(qa(el, '.term-prompt').length).toBe(2)
    expect(lines(el)[0].textContent).toBe('$ sema compile app.sema')
  })

  it('detects a custom prompt (REPL)', async () => {
    document.body.innerHTML = `<sema-terminal prompt="sema>">sema> (factorial 10)\n3628800</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => lines(el).length >= 2)
    expect(q(el, '.term-prompt')!.textContent).toBe('sema>')
    expect(lines(el)[0].textContent).toBe('sema> (factorial 10)')
    expect(q(el, '.term-output')!.textContent).toBe('3628800')
  })

  it('renders a comment-only line without a prompt', async () => {
    document.body.innerHTML = `<sema-terminal># just a note</sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => q(el, '.tok-comment'))
    expect(q(el, '.tok-comment')!.textContent).toBe('# just a note')
    expect(q(el, '.term-prompt')).toBeNull()
  })

  it('dedents indented slotted source', async () => {
    document.body.innerHTML = `<sema-terminal>
      $ git clone repo
      $ cd repo
    </sema-terminal>`
    const el = term()
    await el.updateComplete
    await waitFor(() => lines(el).length >= 2)
    expect(lines(el)[0].textContent).toBe('$ git clone repo')
    expect(lines(el)[1].textContent).toBe('$ cd repo')
  })

  it('escapes HTML in command text (no markup injection)', async () => {
    const el = document.createElement('sema-terminal') as SemaTerminal
    el.textContent = `$ echo "<b>x</b>"`
    document.body.appendChild(el)
    await el.updateComplete
    await waitFor(() => lines(el).length >= 1)
    expect(q(el, 'b')).toBeNull()
    expect(lines(el)[0].textContent).toContain('<b>x</b>')
  })

  it('renders a copy button when copy is set', async () => {
    document.body.innerHTML = `<sema-terminal copy>$ sema run</sema-terminal>`
    const el = term()
    await el.updateComplete
    expect(q(el, 'button[part="copy-button"]')).toBeTruthy()
  })
})
