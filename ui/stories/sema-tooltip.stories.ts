import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-tooltip.js';
import '../src/lib/sema-button.js';

const meta: Meta = {
  title: 'Components/Tooltip',
  component: 'sema-tooltip',
  argTypes: {
    placement: {
      control: 'select',
      options: ['top', 'bottom', 'left', 'right'],
    },
    content: { control: 'text' },
  },
};
export default meta;

type Story = StoryObj;

export const Top: Story = {
  args: { placement: 'top', content: 'Tooltip on top' },
  render: (args) => `
    <sema-tooltip content="${args.content}" placement="${args.placement}">
      <sema-button variant="secondary">Hover me</sema-button>
    </sema-tooltip>`,
};

export const Bottom: Story = {
  args: { placement: 'bottom', content: 'Tooltip below' },
  render: (args) => `
    <sema-tooltip content="${args.content}" placement="${args.placement}">
      <sema-button variant="secondary">Hover me</sema-button>
    </sema-tooltip>`,
};

export const OnIcon: Story = {
  args: { placement: 'top', content: 'Run cell (Shift+Enter)' },
  render: (args) => `
    <sema-tooltip content="${args.content}" placement="${args.placement}">
      <sema-button variant="icon" aria-label="Run">▶</sema-button>
    </sema-tooltip>`,
};

export const Empty: Story = {
  render: () => `
    <sema-tooltip content="" placement="top">
      <sema-button variant="secondary">No tooltip</sema-button>
    </sema-tooltip>`,
};
