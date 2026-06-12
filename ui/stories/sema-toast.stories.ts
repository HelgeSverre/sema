import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-button.js';
import { toast } from '../src/internal/toast.js';

const meta: Meta = {
  title: 'Components/Toast',
};
export default meta;

type Story = StoryObj;

// Stories render trigger buttons; clicking dispatches a toast via the imperative API.
export const Variants: Story = {
  render: () => {
    queueMicrotask(() => {
      document.getElementById('t-info')?.addEventListener('click', () => toast('Heads up — something happened.'));
      document.getElementById('t-success')?.addEventListener('click', () => toast.success('Published v1.2.0'));
      document.getElementById('t-error')?.addEventListener('click', () => toast.error('Upload failed — try again.'));
      document.getElementById('t-warning')?.addEventListener('click', () => toast.warning('Token expires in 3 days.'));
      document.getElementById('t-sticky')?.addEventListener('click', () =>
        toast('Sticky — dismiss me manually.', { duration: null }),
      );
      document.getElementById('t-all')?.addEventListener('click', () => toast.dismissAll());
    });
    return `
      <div style="display:flex; gap:0.5rem; flex-wrap:wrap;">
        <sema-button id="t-info" variant="secondary">Info</sema-button>
        <sema-button id="t-success" variant="secondary">Success</sema-button>
        <sema-button id="t-error" variant="secondary">Error</sema-button>
        <sema-button id="t-warning" variant="secondary">Warning</sema-button>
        <sema-button id="t-sticky" variant="ghost">Sticky</sema-button>
        <sema-button id="t-all" variant="ghost">Dismiss all</sema-button>
      </div>`;
  },
};
