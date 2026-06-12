import { describe, expect, it } from 'vitest'
import { CopyController } from '../src/internal/controllers/copy.js'

function stubClipboard(): { writes: string[]; restore: () => void } {
  const writes: string[] = []
  const orig = Object.getOwnPropertyDescriptor(Navigator.prototype, 'clipboard')
  Object.defineProperty(navigator, 'clipboard', {
    configurable: true,
    value: { writeText: async (t: string) => void writes.push(t) },
  })
  return {
    writes,
    restore: () => {
      if (orig) Object.defineProperty(Navigator.prototype, 'clipboard', orig)
    },
  }
}

describe('CopyController', () => {
  it('writes the provided text and toggles `copied`, then resets', async () => {
    const { writes, restore } = stubClipboard()
    let updates = 0
    // Minimal ReactiveControllerHost stub.
    const host = {
      addController() {},
      removeController() {},
      requestUpdate() {
        updates++
      },
      updateComplete: Promise.resolve(true),
    }
    const ctl = new CopyController(host as never, () => 'hello world', 40)

    expect(ctl.copied).toBe(false)
    await ctl.copy()
    expect(writes).toEqual(['hello world'])
    expect(ctl.copied).toBe(true)
    expect(updates).toBeGreaterThan(0)

    await new Promise((r) => setTimeout(r, 70))
    expect(ctl.copied).toBe(false)

    restore()
  })
})
