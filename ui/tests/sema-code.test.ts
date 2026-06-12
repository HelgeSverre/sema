import { beforeEach, afterEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-code.js'
import { SemaCode } from '../src/lib/sema-code.js'
import { dedent } from '../src/internal/dedent.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

/** Poll until `fn()` is truthy (for the async Shiki highlight pass). */
async function waitFor(fn: () => unknown, timeout = 4000): Promise<void> {
  const start = performance.now()
  while (performance.now() - start < timeout) {
    if (fn()) return
    await new Promise((r) => setTimeout(r, 25))
  }
  throw new Error('waitFor timed out')
}

function codeEl(host: SemaCode) {
  return host.shadowRoot!.querySelector('code')!
}
function lineEls(host: SemaCode) {
  return Array.from(host.shadowRoot!.querySelectorAll('.cl'))
}

describe('dedent', () => {
  it('strips common leading indentation, preserves relative indentation', () => {
    expect(dedent('\n      (define x\n        42)\n    ')).toBe('(define x\n  42)')
  })

  it('trims leading and trailing blank lines', () => {
    expect(dedent('\n\n  a\n  b\n\n  ')).toBe('a\nb')
  })

  it('leaves unindented input unchanged', () => {
    expect(dedent('(a)\n(b)')).toBe('(a)\n(b)')
  })

  it('keeps blank interior lines (does not let them zero the minimum)', () => {
    expect(dedent('\n    a\n\n    b\n')).toBe('a\n\nb')
  })

  it('returns empty string for whitespace-only input', () => {
    expect(dedent('\n   \n  \n')).toBe('')
  })
})

describe('SemaCode', () => {
  afterEach(() => {
    SemaCode.formatter = undefined
  })

  it('dedents slotted source: first line flush, relative indent preserved', async () => {
    document.body.innerHTML = `<sema-code no-highlight>
      (define x
        42)
    </sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => lineEls(el).length >= 2)
    const lines = lineEls(el)
    expect(lines[0].textContent).toBe('(define x')
    expect(lines[1].textContent).toBe('  42)')
  })

  it('preserves indentation in no-dedent mode', async () => {
    document.body.innerHTML = `<sema-code no-dedent no-highlight>
      (a)
    </sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => lineEls(el)[0]?.textContent === '      (a)')
    expect(lineEls(el)[0].textContent).toBe('      (a)')
  })

  it('emits tok-* classes for a Sema snippet', async () => {
    document.body.innerHTML = `<sema-code>(define x 42)</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelector('.tok-keyword'))
    expect(el.shadowRoot!.querySelector('.tok-keyword')!.textContent).toBe('define')
    expect(el.shadowRoot!.querySelector('.tok-number')!.textContent).toBe('42')
    expect(codeEl(el).textContent).toBe('(define x 42)')
  })

  it('does not emit tok-* spans when no-highlight is set', async () => {
    document.body.innerHTML = `<sema-code no-highlight>(define x 42)</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => lineEls(el).length >= 1)
    // give any (incorrectly scheduled) highlight a chance to run
    await new Promise((r) => setTimeout(r, 150))
    expect(el.shadowRoot!.querySelector('[class^="tok-"]')).toBeNull()
    expect(codeEl(el).textContent).toBe('(define x 42)')
  })

  it('renders plain text for an unsupported language', async () => {
    document.body.innerHTML = `<sema-code lang="nonexistent-lang">{"a":1}</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => codeEl(el).textContent!.includes('{'))
    await new Promise((r) => setTimeout(r, 150))
    expect(el.shadowRoot!.querySelector('[class^="tok-"]')).toBeNull()
    expect(codeEl(el).textContent).toBe('{"a":1}')
  })

  it('escapes HTML in source (no markup injection)', async () => {
    // Set textContent directly so the `<b>` survives as literal text rather than
    // being parsed into a real element by the HTML parser (as innerHTML would).
    const el = document.createElement('sema-code') as SemaCode
    el.setAttribute('no-highlight', '')
    el.textContent = '(html "<b>x</b>")'
    document.body.appendChild(el)
    await el.updateComplete
    await waitFor(() => codeEl(el).textContent!.includes('<b>'))
    expect(codeEl(el).querySelector('b')).toBeNull()
    expect(codeEl(el).textContent).toContain('<b>x</b>')
  })

  it('runs the registered formatter before rendering when format is set', async () => {
    let called = ''
    SemaCode.formatter = (code) => {
      called = code
      return 'FORMATTED_OUTPUT'
    }
    document.body.innerHTML = `<sema-code format no-highlight>(  define  x  42  )</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => codeEl(el).textContent === 'FORMATTED_OUTPUT')
    expect(called).toBe('(  define  x  42  )')
    expect(codeEl(el).textContent).toBe('FORMATTED_OUTPUT')
  })

  it('reflects lang and renders a copy button when copy is set', async () => {
    document.body.innerHTML = `<sema-code copy>(a)</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    expect(el.getAttribute('lang')).toBe('sema')
    expect(el.shadowRoot!.querySelector('button[part="copy-button"]')).toBeTruthy()
  })
})

describe('SemaCode — languages', () => {
  it('highlights JSON (keys, numbers, booleans)', async () => {
    document.body.innerHTML = `<sema-code lang="json">{ "n": 42, "ok": true }</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelector('.tok-property'))
    const sr = el.shadowRoot!
    expect(sr.querySelector('.tok-property')!.textContent).toContain('n')
    expect(sr.querySelector('.tok-number')!.textContent).toBe('42')
    expect(sr.querySelector('.tok-boolean')!.textContent).toBe('true')
  })

  it('resolves aliases (bash → shellscript)', async () => {
    document.body.innerHTML = `<sema-code lang="bash">ls -la # list files</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelector('.tok-comment'))
    const comment = Array.from(el.shadowRoot!.querySelectorAll('.tok-comment'))
      .map((e) => e.textContent)
      .join('')
    expect(comment).toContain('# list files')
  })

  it('highlights a registered custom grammar', async () => {
    SemaCode.registerLanguage({
      name: 'toy',
      scopeName: 'source.toy',
      patterns: [{ match: '\\bFOO\\b', name: 'keyword.control.toy' }],
      repository: {},
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
    document.body.innerHTML = `<sema-code lang="toy">FOO bar</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelector('.tok-keyword'))
    expect(el.shadowRoot!.querySelector('.tok-keyword')!.textContent).toBe('FOO')
  })

  it('highlights via a registered lazy loader', async () => {
    SemaCode.registerLanguage('python', () => import('@shikijs/langs/python'))
    document.body.innerHTML = `<sema-code lang="python">x = 1  # set x</sema-code>`
    const el = document.querySelector('sema-code') as SemaCode
    await el.updateComplete
    await waitFor(() => el.shadowRoot!.querySelector('.tok-comment'))
    const comment = Array.from(el.shadowRoot!.querySelectorAll('.tok-comment'))
      .map((e) => e.textContent)
      .join('')
    expect(comment).toContain('# set x')
  })
})
