import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-spinner.js';

const meta: Meta = {
  title: 'Components/Spinner',
  component: 'sema-spinner',
  argTypes: {
    size: { control: 'select', options: ['sm', 'md', 'lg'] },
    label: { control: 'text' },
  },
};
export default meta;

type Story = StoryObj;

export const Default: Story = {
  args: { size: 'md', label: 'Loading' },
  render: (args) => `<sema-spinner size="${args.size}" label="${args.label}"></sema-spinner>`,
};

export const Sizes: Story = {
  render: () => `
    <div style="display:flex; gap:1rem; align-items:center;">
      <sema-spinner size="sm"></sema-spinner>
      <sema-spinner size="md"></sema-spinner>
      <sema-spinner size="lg"></sema-spinner>
    </div>`,
};

/** Pairs with a label inline, the common "loading…" affordance. */
export const WithText: Story = {
  render: () => `
    <span style="display:inline-flex; gap:0.5rem; align-items:center;
                 font-family:var(--mono); font-size:0.8rem; color:var(--text-secondary);">
      <sema-spinner size="sm"></sema-spinner> Compiling…
    </span>`,
};
