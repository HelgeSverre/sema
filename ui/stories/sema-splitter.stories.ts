import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-splitter.js';

const meta: Meta = {
  title: 'Components/Splitter',
  component: 'sema-splitter',
  argTypes: {
    direction: { control: 'select', options: ['horizontal', 'vertical'] },
    step: { control: 'number' },
    shiftStep: { control: 'number' },
  },
};
export default meta;

type Story = StoryObj;

export const Horizontal: Story = {
  render: () => `
    <div style="display:flex;width:400px;height:100px;background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;overflow:hidden;">
      <div style="width:200px;display:flex;align-items:center;justify-content:center;font-family:monospace;font-size:0.65rem;color:var(--text-tertiary,#5a5448);flex-shrink:0">Panel A</div>
      <sema-splitter direction="horizontal"></sema-splitter>
      <div style="flex:1;display:flex;align-items:center;justify-content:center;font-family:monospace;font-size:0.65rem;color:var(--text-tertiary,#5a5448)">Panel B</div>
    </div>`,
};

export const Vertical: Story = {
  render: () => `
    <div style="display:flex;flex-direction:column;width:300px;height:120px;background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;overflow:hidden;">
      <div style="height:60px;display:flex;align-items:center;justify-content:center;font-family:monospace;font-size:0.65rem;color:var(--text-tertiary,#5a5448);flex-shrink:0">Output</div>
      <sema-splitter direction="vertical"></sema-splitter>
      <div style="flex:1;display:flex;align-items:center;justify-content:center;font-family:monospace;font-size:0.65rem;color:var(--text-tertiary,#5a5448)">Files</div>
    </div>`,
};
