import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-grid.js';
import '../src/lib/sema-sidebar.js';

const meta: Meta = {
  title: 'Components/Grid',
  component: 'sema-grid',
  argTypes: {
    min: { control: 'text' },
    cols: { control: 'select', options: [2] },
    gap: { control: 'select', options: ['none', 'xs', 'sm', 'md', 'lg', 'xl', '2xl', '3xl', '4xl'] },
  },
};
export default meta;

type Story = StoryObj;

const itemStyle =
  'background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:0.75rem;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-secondary,#a09888);';

const item = (label: string) => `<div style="${itemStyle}">${label}</div>`;

const caption = (text: string) =>
  `<p style="margin:0.75rem 0 0;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-tertiary,#5a5448);">${text}</p>`;

const resizeBox = (inner: string, width: string) =>
  `<div style="resize:horizontal;overflow:auto;width:${width};max-width:100%;padding:1rem;border:1px dashed var(--border-focus,#333);border-radius:4px;">${inner}</div>`;

export const AutoFill: Story = {
  args: { min: '240px' },
  render: (args) =>
    resizeBox(
      `
      <sema-grid min="${args.min}">
        ${item('string/split')}
        ${item('string/join')}
        ${item('json/encode')}
        ${item('json/decode')}
        ${item('http/get')}
        ${item('file/read')}
      </sema-grid>
      ${caption(`auto-fill columns at least ${args.min} wide — drag the corner and the tracks reflow`)}`,
      '600px',
    ),
};

export const FixedColumns: Story = {
  render: () =>
    resizeBox(
      `
      <sema-grid cols="2" gap="xl">
        ${item('tree-walker backend — trampoline TCO, special forms')}
        ${item('bytecode VM backend — lowering, optimization, dispatch')}
      </sema-grid>
      ${caption('cols="2" — two equal tracks, collapsing to one below a 700px container width (container query, not viewport)')}`,
      '760px',
    ),
};

export const GapScale: Story = {
  render: () => `
    <div style="display:flex;flex-direction:column;gap:1.5rem;max-width:480px;">
      ${(['none', 'sm', 'md', 'xl', '2xl'] as const)
        .map(
          (g) => `
      <div>
        <div style="font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-tertiary,#5a5448);margin-bottom:0.5rem;">gap="${g}"</div>
        <sema-grid cols="2" gap="${g}">
          ${item('(car pair)')}
          ${item('(cdr pair)')}
        </sema-grid>
      </div>`,
        )
        .join('')}
    </div>`,
};

export const SpannedItem: Story = {
  render: () => `
    <sema-grid min="180px" style="max-width:600px;">
      <div style="grid-column: span 2; ${itemStyle}">grid-column: span 2 — plain page CSS on the light-DOM child, no API needed</div>
      ${item('regex/match?')}
      ${item('path/join')}
      ${item('string/trim')}
    </sema-grid>`,
};

export const NestedInSidebar: Story = {
  render: () => `
    <sema-sidebar side-width="16rem" gap="xl" style="border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:1rem;">
      <div slot="aside">
        <div style="font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-tertiary,#5a5448);margin-bottom:0.5rem;">min-grid inside the aside — the min(…, 100%) clamp prevents overflow</div>
        <sema-grid min="200px" gap="sm">
          ${item('regex/match?')}
          ${item('string/trim')}
          ${item('keyword->string')}
        </sema-grid>
      </div>
      <div>${item('content pane — the grid above responds to its own width, not the viewport')}</div>
    </sema-sidebar>`,
};
