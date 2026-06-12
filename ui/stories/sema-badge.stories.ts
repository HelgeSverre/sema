import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-badge.js';

const meta: Meta = {
  title: 'Components/Badge',
  component: 'sema-badge',
  argTypes: {
    variant: {
      control: 'select',
      options: ['neutral', 'gold', 'success', 'error'],
    },
    pill: { control: 'boolean' },
    dot: { control: 'boolean' },
  },
};
export default meta;

type Story = StoryObj;

export const Default: Story = {
  args: { variant: 'neutral' },
  render: (args) =>
    `<sema-badge variant="${args.variant}" ?pill=${args.pill} ?dot=${args.dot}>badge</sema-badge>`,
};

export const Variants: Story = {
  render: () => `
    <div style="display:flex; gap:0.5rem; align-items:center;">
      <sema-badge variant="neutral">neutral</sema-badge>
      <sema-badge variant="gold">gold</sema-badge>
      <sema-badge variant="success">success</sema-badge>
      <sema-badge variant="error">error</sema-badge>
    </div>`,
};

export const StatusDots: Story = {
  render: () => `
    <div style="display:flex; gap:0.5rem; align-items:center;">
      <sema-badge variant="success" dot>online</sema-badge>
      <sema-badge variant="error" dot>offline</sema-badge>
      <sema-badge variant="gold" dot>running</sema-badge>
    </div>`,
};

/** Pill variant in a wrapping container — covers the old Provider Pill List. */
export const ProviderPills: Story = {
  render: () => `
    <div style="display:flex; flex-wrap:wrap; gap:0.4rem; max-width:320px;">
      <sema-badge pill>Anthropic</sema-badge>
      <sema-badge pill>OpenAI</sema-badge>
      <sema-badge pill>Gemini</sema-badge>
      <sema-badge pill>Ollama</sema-badge>
      <sema-badge pill variant="gold">Default</sema-badge>
    </div>`,
};
