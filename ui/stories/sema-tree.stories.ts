import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-tree.js';

const meta: Meta = {
  title: 'Components/Tree',
  component: 'sema-tree',
};
export default meta;

type Story = StoryObj;

export const Examples: Story = {
  render: () => `
    <div style="width:240px;background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:0.5rem 0;">
      <sema-tree>
        <sema-tree-item label="getting-started" expanded has-children>
          <sema-tree-item label="hello.sema"></sema-tree-item>
          <sema-tree-item label="repl.sema"></sema-tree-item>
        </sema-tree-item>
        <sema-tree-item label="functional" has-children>
          <sema-tree-item label="map-filter.sema"></sema-tree-item>
          <sema-tree-item label="fold-reduce.sema"></sema-tree-item>
        </sema-tree-item>
        <sema-tree-item label="data" has-children>
          <sema-tree-item label="json-parse.sema"></sema-tree-item>
        </sema-tree-item>
        <sema-tree-item label="http" has-children>
          <sema-tree-item label="fetch-api.sema"></sema-tree-item>
          <sema-tree-item label="post-json.sema"></sema-tree-item>
        </sema-tree-item>
      </sema-tree>
    </div>`,
};
