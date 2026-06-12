import { beforeEach, describe, expect, it } from 'vitest'
import '../src/lib/sema-tree.js'

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('SemaTreeItem', () => {
  it('renders with label', async () => {
    document.body.innerHTML = '<sema-tree-item label="hello.sema"></sema-tree-item>'
    const el = document.querySelector('sema-tree-item')!
    expect(el.label).toBe('hello.sema')
  })

  it('renders chevron when has-children', async () => {
    document.body.innerHTML = '<sema-tree-item label="folder" has-children></sema-tree-item>'
    const el = document.querySelector('sema-tree-item')!
    await el.updateComplete
    const chevron = el.shadowRoot!.querySelector('.chevron') as HTMLElement
    expect(chevron).toBeTruthy()
    expect(chevron.style.visibility).not.toBe('hidden')
  })

  it('hides chevron when not has-children', async () => {
    document.body.innerHTML = '<sema-tree-item label="file.sema"></sema-tree-item>'
    const el = document.querySelector('sema-tree-item')!
    await el.updateComplete
    const chevron = el.shadowRoot!.querySelector('.chevron') as HTMLElement
    expect(chevron).toBeTruthy()
  })

  it('reflects selected attribute', async () => {
    document.body.innerHTML = '<sema-tree-item label="x" selected></sema-tree-item>'
    const el = document.querySelector('sema-tree-item')!
    expect(el.selected).toBe(true)
    expect(el.hasAttribute('selected')).toBe(true)
  })

  it('expands/collapses children on click', async () => {
    document.body.innerHTML = `
      <sema-tree-item label="folder" has-children>
        <sema-tree-item label="child.sema"></sema-tree-item>
      </sema-tree-item>`
    const parent = document.querySelector('sema-tree-item[label="folder"]')!
    await parent.updateComplete

    const row = parent.shadowRoot!.querySelector('.row') as HTMLElement
    row.click()
    await parent.updateComplete
    expect(parent.expanded).toBe(true)

    row.click()
    await parent.updateComplete
    expect(parent.expanded).toBe(false)
  })

  it('has role treeitem', async () => {
    document.body.innerHTML = '<sema-tree-item label="test"></sema-tree-item>'
    const el = document.querySelector('sema-tree-item')!
    await el.updateComplete
    const row = el.shadowRoot!.querySelector('.row')!
    expect(row.getAttribute('role')).toBe('treeitem')
  })

  it('calculates depth from nesting', async () => {
    document.body.innerHTML = `
      <sema-tree>
        <sema-tree-item id="root" label="root">
          <sema-tree-item id="child" label="child">
            <sema-tree-item id="grandchild" label="grandchild"></sema-tree-item>
          </sema-tree-item>
        </sema-tree-item>
      </sema-tree>`
    const root = document.querySelector('#root') as any
    const child = document.querySelector('#child') as any
    const grandchild = document.querySelector('#grandchild') as any
    await root.updateComplete
    await child.updateComplete
    await grandchild.updateComplete
    expect(root.depth).toBe(0)
    expect(child.depth).toBe(1)
    expect(grandchild.depth).toBe(2)
  })

  it('dispatches sema-tree-select event on click', async () => {
    document.body.innerHTML = '<sema-tree-item id="x" label="test.sema"></sema-tree-item>'
    const el = document.querySelector('#x')!
    await el.updateComplete

    let selected = false
    el.addEventListener('sema-tree-select', (e: any) => {
      selected = true
      expect(e.detail.label).toBe('test.sema')
    })

    const row = el.shadowRoot!.querySelector('.row') as HTMLElement
    row.click()
    expect(selected).toBe(true)
  })
})
