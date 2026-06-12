import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-tabs.js';
import '../src/lib/sema-code.js';

const meta: Meta = {
  title: 'Components/Tabs',
  component: 'sema-tabs',
};
export default meta;

type Story = StoryObj;

const panelStyle = 'font-family:var(--mono, monospace); font-size:0.75rem; color:var(--text-secondary, #a09888);';

export const Default: Story = {
  render: () => `
    <sema-tabs style="max-width:480px;">
      <sema-tab value="readme">Readme</sema-tab>
      <sema-tab value="versions">Versions</sema-tab>
      <sema-tab value="deps">Dependencies</sema-tab>
      <sema-tab-panel value="readme"><p style="${panelStyle}">A Lisp for the LLM era. Install with cargo or homebrew.</p></sema-tab-panel>
      <sema-tab-panel value="versions"><p style="${panelStyle}">0.6.0 · 0.5.2 · 0.5.1 · 0.5.0</p></sema-tab-panel>
      <sema-tab-panel value="deps"><p style="${panelStyle}">No runtime dependencies.</p></sema-tab-panel>
    </sema-tabs>`,
};

export const WithBadges: Story = {
  render: () => `
    <sema-tabs style="max-width:480px;">
      <sema-tab value="readme">Readme</sema-tab>
      <sema-tab value="versions">Versions <span style="color:var(--text-tertiary, #5a5448);">12</span></sema-tab>
      <sema-tab value="deps">Dependencies <span style="color:var(--text-tertiary, #5a5448);">3</span></sema-tab>
      <sema-tab-panel value="readme"><p style="${panelStyle}">Labels take arbitrary inline content — counts, dots, icons.</p></sema-tab-panel>
      <sema-tab-panel value="versions"><p style="${panelStyle}">Twelve published versions.</p></sema-tab-panel>
      <sema-tab-panel value="deps"><p style="${panelStyle}">Three dependencies.</p></sema-tab-panel>
    </sema-tabs>`,
};

export const DisabledTab: Story = {
  render: () => `
    <sema-tabs style="max-width:480px;">
      <sema-tab value="readme">Readme</sema-tab>
      <sema-tab value="versions" disabled>Versions</sema-tab>
      <sema-tab value="deps">Dependencies</sema-tab>
      <sema-tab-panel value="readme"><p style="${panelStyle}">Arrow keys skip the disabled tab.</p></sema-tab-panel>
      <sema-tab-panel value="versions"><p style="${panelStyle}">unreachable</p></sema-tab-panel>
      <sema-tab-panel value="deps"><p style="${panelStyle}">Dependencies list.</p></sema-tab-panel>
    </sema-tabs>`,
};

export const ManualActivation: Story = {
  render: () => `
    <p style="${panelStyle} margin-bottom:0.75rem;">activation="manual": arrows move focus only; Enter/Space selects. (Contrast: toggle-group is always manual — radiogroup convention.)</p>
    <sema-tabs activation="manual" style="max-width:480px;">
      <sema-tab value="js">JavaScript</sema-tab>
      <sema-tab value="rust">Rust</sema-tab>
      <sema-tab value="sema">Sema</sema-tab>
      <sema-tab-panel value="js"><p style="${panelStyle}">const x = 42;</p></sema-tab-panel>
      <sema-tab-panel value="rust"><p style="${panelStyle}">let x: u32 = 42;</p></sema-tab-panel>
      <sema-tab-panel value="sema"><p style="${panelStyle}">(define x 42)</p></sema-tab-panel>
    </sema-tabs>`,
};

export const PreselectedTab: Story = {
  render: () => `
    <sema-tabs style="max-width:480px;">
      <sema-tab value="readme">Readme</sema-tab>
      <sema-tab value="versions" selected>Versions</sema-tab>
      <sema-tab-panel value="readme"><p style="${panelStyle}">readme</p></sema-tab-panel>
      <sema-tab-panel value="versions"><p style="${panelStyle}">Selected via the selected hint on the tab (no group value).</p></sema-tab-panel>
    </sema-tabs>`,
};

export const OverflowingTablist: Story = {
  render: () => `
    <sema-tabs style="max-width:360px;">
      ${Array.from({ length: 14 }, (_, i) => `<sema-tab value="v0-${13 - i}">v0.${13 - i}.0</sema-tab>`).join('')}
      ${Array.from({ length: 14 }, (_, i) => `<sema-tab-panel value="v0-${13 - i}"><p style="${panelStyle}">Release notes for v0.${13 - i}.0 — the tablist scrolls and keeps the active tab in view.</p></sema-tab-panel>`).join('')}
    </sema-tabs>`,
};

export const CodePanels: Story = {
  render: () => `
    <sema-tabs activation="manual" style="max-width:540px;">
      <sema-tab value="brew">brew</sema-tab>
      <sema-tab value="cargo">cargo</sema-tab>
      <sema-tab value="curl">curl</sema-tab>
      <sema-tab-panel value="brew"><sema-code language="shellscript">brew install helgesverre/tap/sema-lang</sema-code></sema-tab-panel>
      <sema-tab-panel value="cargo"><sema-code language="shellscript">cargo install sema-lang</sema-code></sema-tab-panel>
      <sema-tab-panel value="curl"><sema-code language="shellscript">curl -fsSL https://sema-lang.com/install.sh | sh</sema-code></sema-tab-panel>
    </sema-tabs>`,
};
