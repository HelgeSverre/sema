import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-drawer.js';
import '../src/lib/sema-button.js';

const meta: Meta = {
  title: 'Components/Drawer',
  component: 'sema-drawer',
  argTypes: {
    placement: { control: 'select', options: ['left', 'right', 'top', 'bottom'] },
  },
};
export default meta;

type Story = StoryObj;

const open = (id: string) => `(function(){document.getElementById('${id}').show()})()`;

export const Playground: Story = {
  args: { placement: 'right' },
  render: (args) => `
    <sema-button variant="secondary" onclick="${open('drawer-pg')}">Open drawer</sema-button>
    <sema-drawer id="drawer-pg" placement="${args.placement}" label="Settings">
      <p style="font-family:var(--serif); color:var(--text-secondary);">
        Drawer body content. Dock it to any edge with <code>placement</code>.
      </p>
      <sema-button slot="footer" variant="secondary"
        onclick="this.closest('sema-drawer').close()">Close</sema-button>
    </sema-drawer>`,
};

export const Placements: Story = {
  render: () => `
    <div style="display:flex; gap:0.5rem; flex-wrap:wrap;">
      ${['left', 'right', 'top', 'bottom']
        .map(
          (p) => `
        <sema-button variant="secondary" onclick="${open(`drawer-${p}`)}">${p}</sema-button>
        <sema-drawer id="drawer-${p}" placement="${p}" label="${p} drawer">
          <p style="font-family:var(--serif); color:var(--text-secondary);">Docked to the ${p} edge.</p>
        </sema-drawer>`,
        )
        .join('')}
    </div>`,
};

/** Custom size via the --drawer-size custom property. */
export const CustomSize: Story = {
  render: () => `
    <sema-button variant="secondary" onclick="${open('drawer-wide')}">Open wide drawer</sema-button>
    <sema-drawer id="drawer-wide" placement="right" label="Wide" style="--drawer-size: 520px;">
      <p style="font-family:var(--serif); color:var(--text-secondary);">A 520px-wide panel.</p>
    </sema-drawer>`,
};
