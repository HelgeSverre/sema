import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-popover.js';
import '../src/lib/sema-menu.js';
import '../src/lib/sema-button.js';

const meta: Meta = {
  title: 'Components/Popover',
  component: 'sema-popover',
  argTypes: {
    placement: {
      control: 'select',
      options: ['bottom-start', 'bottom-end', 'top-start', 'top-end', 'left', 'right'],
    },
    'open-on': { control: 'select', options: ['click', 'hover'] },
  },
};
export default meta;

type Story = StoryObj;

export const Menu: Story = {
  args: { placement: 'bottom-start' },
  render: (args) => `
    <sema-popover placement="${args.placement}" open-on="${args['open-on'] || 'click'}">
      <sema-button slot="trigger" variant="secondary">Insert ▾</sema-button>
      <sema-menu>
        <sema-menu-item value="code">Code cell</sema-menu-item>
        <sema-menu-item value="markdown">Markdown cell</sema-menu-item>
        <sema-menu-item value="divider" disabled>Divider (disabled)</sema-menu-item>
      </sema-menu>
    </sema-popover>`,
};

export const ArbitraryContent: Story = {
  render: () => `
    <sema-popover placement="bottom-start">
      <sema-button slot="trigger" variant="pill">Filters</sema-button>
      <div style="padding:0.5rem 0.75rem; font-family:var(--mono); font-size:0.75rem; color:var(--text-secondary); max-width:16rem;">
        Any element works as popover content — not just menus.
      </div>
    </sema-popover>`,
};

export const Hover: Story = {
  render: () => `
    <sema-popover placement="bottom-start" open-on="hover">
      <sema-button slot="trigger" variant="ghost">Hover me</sema-button>
      <sema-menu>
        <sema-menu-item value="a">Option A</sema-menu-item>
        <sema-menu-item value="b">Option B</sema-menu-item>
      </sema-menu>
    </sema-popover>`,
};
