import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-toggle.js';
import '../src/lib/sema-toggle-group.js';

const meta: Meta = {
  title: 'Components/ToggleGroup',
  component: 'sema-toggle-group',
};
export default meta;

type Story = StoryObj;

export const EngineSelector: Story = {
  render: () => `
    <sema-toggle-group value="tw">
      <sema-toggle value="tw">Tree-walker</sema-toggle>
      <sema-toggle value="vm">Bytecode VM</sema-toggle>
    </sema-toggle-group>`,
};

export const StorageBackend: Story = {
  render: () => `
    <sema-toggle-group value="memory">
      <sema-toggle value="memory">Memory</sema-toggle>
      <sema-toggle value="local">Local</sema-toggle>
      <sema-toggle value="session">Session</sema-toggle>
      <sema-toggle value="idb">IndexedDB</sema-toggle>
    </sema-toggle-group>`,
};
