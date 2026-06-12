import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-sidebar.js';
import '../src/lib/sema-splitter.js';

const meta: Meta = {
  title: 'Components/Sidebar',
  component: 'sema-sidebar',
  argTypes: {
    sideWidth: { control: 'text' },
    contentMin: { control: 'text' },
    gap: { control: 'select', options: ['none', 'xs', 'sm', 'md', 'lg', 'xl', '2xl', '3xl', '4xl'] },
  },
};
export default meta;

type Story = StoryObj;

const paneStyle =
  'background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:1rem;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-secondary,#a09888);';

const caption = (text: string) =>
  `<p style="margin:0.75rem 0 0;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-tertiary,#5a5448);">${text}</p>`;

const docsLink = (label: string, active = false) =>
  `<a href="#" style="font-family:var(--mono,monospace);font-size:0.7rem;color:${active ? 'var(--gold,#c8a855)' : 'var(--text-secondary,#a09888)'};text-decoration:none;">${label}</a>`;

export const Default: Story = {
  args: { sideWidth: '16rem', gap: 'xl' },
  render: (args) => `
    <sema-sidebar side-width="${args.sideWidth}" gap="${args.gap}">
      <nav slot="aside" aria-label="Docs" style="display:flex;flex-direction:column;gap:0.5rem;background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:1rem;">
        <span style="font-family:var(--mono,monospace);font-size:0.6rem;letter-spacing:0.1em;text-transform:uppercase;color:var(--gold,#c8a855);">Guide</span>
        ${docsLink('getting-started')}
        ${docsLink('special-forms', true)}
        ${docsLink('pattern-matching')}
        ${docsLink('modules')}
        ${docsLink('async-await')}
      </nav>
      <article>
        <h2 style="font-family:var(--serif,Georgia,serif);font-weight:500;color:var(--text-primary,#d8d0c0);margin:0 0 0.5rem;">Special forms</h2>
        <p style="font-family:var(--serif,Georgia,serif);color:var(--text-secondary,#a09888);margin:0 0 1rem;">
          Tail positions return a trampoline step, so recursive loops run in constant stack space:
        </p>
        <pre style="background:var(--bg-editor,#0a0a0a);border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:0.75rem 1rem;font-family:var(--mono,monospace);font-size:0.7rem;color:var(--text-primary,#d8d0c0);overflow:auto;margin:0;">(defn count-down [n]
  (if (= n 0)
    :done
    (count-down (- n 1))))</pre>
      </article>
    </sema-sidebar>`,
};

export const StackingThreshold: Story = {
  render: () => `
    <div style="resize:horizontal;overflow:auto;width:420px;max-width:100%;padding:1rem;border:1px dashed var(--border-focus,#333);border-radius:4px;">
      <sema-sidebar>
        <div slot="aside" style="${paneStyle}">aside — flex-basis 18rem</div>
        <div style="${paneStyle}">content — min-inline-size 50%; when this pane can no longer hold half the row, the panes wrap to a stack</div>
      </sema-sidebar>
      ${caption('drag the corner wider — the stacking threshold is intrinsic (no media or container queries)')}
    </div>`,
};

export const NoGapBordered: Story = {
  render: () => `
    <sema-sidebar gap="none" style="--sema-sidebar-side: 14rem; border:1px solid var(--border,#1e1e1e);border-radius:4px;background:var(--bg-elevated,#141414);">
      <div slot="aside" style="border-inline-end:1px solid var(--border,#1e1e1e);padding:1rem;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-secondary,#a09888);">
        aside — no side-width attribute, so the inherited --sema-sidebar-side: 14rem wins over the 18rem fallback
      </div>
      <div style="padding:1rem;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-secondary,#a09888);">
        content — gap="none" for border-separated panes
      </div>
    </sema-sidebar>`,
};

export const VsSplitter: Story = {
  render: () => `
    <div style="display:flex;flex-direction:column;gap:1.5rem;max-width:640px;">
      <p style="margin:0;font-family:var(--serif,Georgia,serif);color:var(--text-secondary,#a09888);">
        <code style="font-family:var(--mono,monospace);font-size:0.85em;color:var(--gold,#c8a855);">sema-sidebar</code>
        is the static layout: fixed-basis aside + growing content, stacking intrinsically on narrow
        containers. It never resizes interactively and emits no events.
        <code style="font-family:var(--mono,monospace);font-size:0.85em;color:var(--gold,#c8a855);">sema-splitter</code>
        is only the drag handle — it owns no panes. For drag-resizable panes, place it between app-managed panes.
      </p>
      <div>
        ${caption('static — sema-sidebar')}
        <sema-sidebar side-width="10rem" style="margin-top:0.5rem;">
          <div slot="aside" style="${paneStyle}">nav</div>
          <div style="${paneStyle}">content</div>
        </sema-sidebar>
      </div>
      <div>
        ${caption('interactive — sema-splitter between app-managed panes')}
        <div style="display:flex;width:400px;height:80px;margin-top:0.5rem;background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;overflow:hidden;">
          <div style="width:160px;display:flex;align-items:center;justify-content:center;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-tertiary,#5a5448);flex-shrink:0">Panel A</div>
          <sema-splitter direction="horizontal"></sema-splitter>
          <div style="flex:1;display:flex;align-items:center;justify-content:center;font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-tertiary,#5a5448)">Panel B</div>
        </div>
      </div>
    </div>`,
};
