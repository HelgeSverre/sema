import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-kbd.js';

const meta: Meta = {
  title: 'Components/Kbd',
  component: 'sema-kbd',
  argTypes: {
    keys: { control: 'text' },
  },
};
export default meta;

type Story = StoryObj;

export const SingleKey: Story = {
  render: () => `<sema-kbd>Esc</sema-kbd>`,
};

export const Combo: Story = {
  args: { keys: 'Cmd+Shift+P' },
  render: (args) => `<sema-kbd keys="${args.keys}"></sema-kbd>`,
};

export const InContext: Story = {
  render: () => `
    <div style="display:flex; flex-direction:column; gap:0.5rem;
                font-family:var(--mono); font-size:0.8rem; color:var(--text-secondary);">
      <span>Run cell <sema-kbd keys="Cmd+Enter"></sema-kbd></span>
      <span>Command palette <sema-kbd keys="Cmd+Shift+P"></sema-kbd></span>
      <span>Dismiss <sema-kbd>Esc</sema-kbd></span>
    </div>`,
};
