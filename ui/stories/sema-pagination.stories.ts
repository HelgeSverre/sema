import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-pagination.js';

const meta: Meta = {
  title: 'Components/Pagination',
  component: 'sema-pagination',
  argTypes: {
    page: { control: { type: 'number', min: 1 } },
    total: { control: { type: 'number', min: 1 } },
    siblings: { control: { type: 'number', min: 0 } },
    boundaries: { control: { type: 'number', min: 1 } },
  },
};
export default meta;

type Story = StoryObj;

export const Default: Story = {
  args: { page: 5, total: 10, siblings: 1, boundaries: 1 },
  render: (args) => `
    <sema-pagination
      page="${args.page}"
      total="${args.total}"
      siblings="${args.siblings}"
      boundaries="${args.boundaries}"></sema-pagination>`,
};

export const NearStart: Story = {
  render: () => `<sema-pagination page="1" total="20"></sema-pagination>`,
};

export const NearEnd: Story = {
  render: () => `<sema-pagination page="20" total="20"></sema-pagination>`,
};

export const NoTruncation: Story = {
  render: () => `<sema-pagination page="3" total="5"></sema-pagination>`,
};

/** Self-contained controlled example: the event re-sets the page. */
export const Controlled: Story = {
  render: () => `
    <sema-pagination page="1" total="12"
      onsema-page-change="this.page = event.detail.page"></sema-pagination>`,
};
