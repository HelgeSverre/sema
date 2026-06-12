import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-scroll-area.js';

const meta: Meta = {
  title: 'Components/ScrollArea',
  component: 'sema-scroll-area',
  argTypes: {
    orientation: { control: 'select', options: ['vertical', 'horizontal', 'both'] },
  },
};
export default meta;

type Story = StoryObj;

const lines = Array.from({ length: 30 }, (_, i) => `line ${i + 1}`).join('<br>');

export const Vertical: Story = {
  render: () => `
    <sema-scroll-area style="max-height:10rem; width:240px; border:1px solid var(--border); border-radius:6px; padding:0.5rem; font-family:var(--mono); font-size:0.75rem; color:var(--text-secondary);">
      ${lines}
    </sema-scroll-area>`,
};

export const Horizontal: Story = {
  render: () => `
    <sema-scroll-area orientation="horizontal" style="width:240px; border:1px solid var(--border); border-radius:6px; padding:0.5rem; font-family:var(--mono); font-size:0.75rem; color:var(--text-secondary);">
      <div style="white-space:nowrap;">a very wide row that overflows horizontally and needs to scroll sideways to read fully →</div>
    </sema-scroll-area>`,
};
