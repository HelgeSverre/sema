import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-button.js';

const meta: Meta = {
  title: 'Components/Button',
  component: 'sema-button',
  argTypes: {
    variant: {
      control: 'select',
      options: ['primary', 'secondary', 'ghost', 'icon', 'pill', 'run', 'debug', 'action'],
    },
    disabled: { control: 'boolean' },
    danger: { control: 'boolean' },
    shortcut: { control: 'text' },
  },
};
export default meta;

type Story = StoryObj;

export const Primary: Story = {
  args: { variant: 'primary' },
  render: (args) => `<sema-button variant="${args.variant}" ?disabled=${args.disabled}>Get Started</sema-button>`,
};

export const Secondary: Story = {
  args: { variant: 'secondary' },
  render: (args) => `<sema-button variant="${args.variant}">GitHub</sema-button>`,
};

export const Ghost: Story = {
  args: { variant: 'ghost' },
  render: (args) => `<sema-button variant="${args.variant}">Cancel</sema-button>`,
};

export const Icon: Story = {
  args: { variant: 'icon' },
  render: () => `<sema-button variant="icon" aria-label="Close">&lt;/&gt;</sema-button>`,
};

export const Pill: Story = {
  args: { variant: 'pill' },
  render: () => `<sema-button variant="pill">+ Code</sema-button>`,
};

export const Run: Story = {
  args: { variant: 'run' },
  render: (args) => `<sema-button variant="run" shortcut="${args.shortcut || '⌘↵'}">Run</sema-button>`,
};

export const Debug: Story = {
  args: { variant: 'debug' },
  render: (args) => `<sema-button variant="debug" ?danger=${args.danger}>▶</sema-button>`,
};

export const Action: Story = {
  args: { variant: 'action' },
  render: (args) => `<sema-button variant="action" ?danger=${args.danger}>▶</sema-button>`,
};
