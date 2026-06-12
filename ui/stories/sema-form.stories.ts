import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-input.js';
import '../src/lib/sema-textarea.js';
import '../src/lib/sema-select.js';
import '../src/lib/sema-field.js';

const meta: Meta = {
  title: 'Components/Form',
};
export default meta;

type Story = StoryObj;

export const Input: Story = {
  render: () => `<sema-input placeholder="your-package-name" style="width:280px;"></sema-input>`,
};

export const Textarea: Story = {
  render: () => `<sema-textarea placeholder="Description…" rows="4" style="width:340px;"></sema-textarea>`,
};

export const Select: Story = {
  name: 'Select (custom dropdown)',
  render: () => `
    <sema-select value="vm" style="width:240px;">
      <option value="tw">Tree-walker</option>
      <option value="vm">Bytecode VM</option>
    </sema-select>`,
};

export const SelectNative: Story = {
  name: 'Select (native flag)',
  render: () => `
    <sema-select native value="vm" style="width:240px;">
      <option value="tw">Tree-walker</option>
      <option value="vm">Bytecode VM</option>
    </sema-select>`,
};

export const Fields: Story = {
  name: 'Fields (label + hint + error)',
  render: () => `
    <div style="display:flex; flex-direction:column; gap:1rem; width:320px;">
      <sema-field label="Package name" hint="Lowercase, hyphen-separated.">
        <sema-input placeholder="http-helpers" name="name"></sema-input>
      </sema-field>
      <sema-field label="Engine">
        <sema-select name="engine" value="vm">
          <option value="tw">Tree-walker</option>
          <option value="vm">Bytecode VM</option>
        </sema-select>
      </sema-field>
      <sema-field label="API token" error="Token is required.">
        <sema-input type="password" name="token"></sema-input>
      </sema-field>
    </div>`,
};
