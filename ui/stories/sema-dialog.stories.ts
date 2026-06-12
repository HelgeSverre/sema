import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-dialog.js';
import '../src/lib/sema-button.js';

const meta: Meta = {
  title: 'Components/Dialog',
  component: 'sema-dialog',
};
export default meta;

type Story = StoryObj;

export const Confirm: Story = {
  render: () => `
    <sema-dialog open label="Confirm Action">
      <p>This is a modal dialog. Press Escape or click the backdrop to close.</p>
      <div slot="footer">
        <sema-button variant="ghost">Cancel</sema-button>
        <sema-button variant="primary">Confirm</sema-button>
      </div>
    </sema-dialog>`,
};

export const Simple: Story = {
  render: () => `
    <sema-dialog open label="Notice">
      <p>Something happened that you should know about.</p>
      <div slot="footer">
        <sema-button variant="primary">OK</sema-button>
      </div>
    </sema-dialog>`,
};
