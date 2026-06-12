import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-container.js';
import '../src/lib/sema-grid.js';
import '../src/lib/sema-sidebar.js';

const meta: Meta = {
  title: 'Components/Container',
  component: 'sema-container',
  argTypes: {
    size: { control: 'select', options: ['md', 'lg', 'full'] },
    gutter: { control: 'select', options: ['none', 'xs', 'sm', 'md', 'lg', 'xl', '2xl', '3xl', '4xl'] },
  },
};
export default meta;

type Story = StoryObj;

const outline = 'outline:1px dashed var(--gold-dim,rgba(200,168,85,0.5));';

const content = (label: string) =>
  `<div style="background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:0.75rem 1rem;font-family:var(--mono,monospace);font-size:0.7rem;color:var(--text-secondary,#a09888);">${label}</div>`;

export const Sizes: Story = {
  render: () => `
    <div style="display:flex;flex-direction:column;gap:1.5rem;">
      <sema-container size="md" style="${outline}">
        ${content('size="md" — max-inline-size 1000px (--container-md)')}
      </sema-container>
      <sema-container style="${outline}">
        ${content('no size attribute — effective default lg, 1200px (--container-lg)')}
      </sema-container>
      <sema-container size="full" style="${outline}">
        ${content('size="full" — unconstrained, responsive gutters only')}
      </sema-container>
    </div>`,
};

export const Gutters: Story = {
  render: () => `
    <div style="display:flex;flex-direction:column;gap:1.5rem;">
      ${(['none', 'md', 'xl', '2xl'] as const)
        .map(
          (g) => `
      <sema-container size="md" gutter="${g}" style="${outline}">
        ${content(`gutter="${g}"${g === 'none' ? ' — full bleed' : ''}`)}
      </sema-container>`,
        )
        .join('')}
      <sema-container size="md" style="${outline}">
        ${content('no gutter attribute — responsive clamp(24px, 4vw, 32px)')}
      </sema-container>
    </div>`,
};

export const CustomMax: Story = {
  render: () => `
    <sema-container style="--sema-container-max: 30rem; ${outline}">
      ${content('--sema-container-max: 30rem — custom-property escape hatch; no size attribute, so the inherited value wins over the 1200px default')}
    </sema-container>`,
};

const stdlibCard = (name: string, sig: string, desc: string) => `
  <div style="background:var(--bg-elevated,#141414);border:1px solid var(--border,#1e1e1e);border-radius:4px;padding:1rem;">
    <div style="font-family:var(--mono,monospace);font-size:0.75rem;color:var(--gold,#c8a855);">${name}</div>
    <div style="font-family:var(--mono,monospace);font-size:0.65rem;color:var(--text-tertiary,#5a5448);margin-top:0.25rem;">${sig}</div>
    <p style="font-family:var(--serif,Georgia,serif);font-size:0.85rem;color:var(--text-secondary,#a09888);margin:0.5rem 0 0;">${desc}</p>
  </div>`;

const docsLink = (label: string, active = false) =>
  `<a href="#" style="font-family:var(--mono,monospace);font-size:0.7rem;color:${active ? 'var(--gold,#c8a855)' : 'var(--text-secondary,#a09888)'};text-decoration:none;">${label}</a>`;

/** Composed page shell: container > sidebar > grid, with Sema docs content. */
export const ComposedPage: Story = {
  render: () => `
    <div style="border:1px solid var(--border,#1e1e1e);border-radius:4px;overflow:hidden;background:var(--bg,#0c0c0c);">
      <nav style="border-bottom:1px solid var(--border,#1e1e1e);padding-block:0.75rem;">
        <sema-container size="full">
          <div style="display:flex;align-items:baseline;gap:1.5rem;">
            <span style="font-family:var(--mono,monospace);font-size:0.75rem;color:var(--gold,#c8a855);">(sema)</span>
            ${docsLink('docs', true)}
            ${docsLink('playground')}
            ${docsLink('github')}
          </div>
        </sema-container>
      </nav>
      <sema-container size="md" style="padding-block:2rem;">
        <sema-sidebar side-width="13rem" gap="2xl">
          <nav slot="aside" aria-label="Docs" style="display:flex;flex-direction:column;gap:0.5rem;">
            <span style="font-family:var(--mono,monospace);font-size:0.6rem;letter-spacing:0.1em;text-transform:uppercase;color:var(--gold,#c8a855);">Standard library</span>
            ${docsLink('getting-started')}
            ${docsLink('special-forms')}
            ${docsLink('string/', true)}
            ${docsLink('file/')}
            ${docsLink('http/')}
            ${docsLink('json/')}
            ${docsLink('regex/')}
          </nav>
          <article>
            <h2 style="font-family:var(--serif,Georgia,serif);font-weight:500;color:var(--text-primary,#d8d0c0);margin:0 0 0.5rem;">Strings &amp; data</h2>
            <p style="font-family:var(--serif,Georgia,serif);color:var(--text-secondary,#a09888);margin:0 0 1.25rem;max-width:var(--container-prose,48rem);">
              All new functions are slash-namespaced. Predicates end in <code style="font-family:var(--mono,monospace);font-size:0.85em;color:var(--gold,#c8a855);">?</code>;
              keywords work as functions, so <code style="font-family:var(--mono,monospace);font-size:0.85em;color:var(--gold,#c8a855);">(:name person)</code> reads a field.
            </p>
            <sema-grid min="220px">
              ${stdlibCard('string/split', '(string/split s sep)', 'Split a string on a separator, returning a list.')}
              ${stdlibCard('json/encode', '(json/encode value)', 'Encode any Sema value as a JSON string.')}
              ${stdlibCard('http/get', '(http/get url)', 'Fetch a URL and return the response map.')}
              ${stdlibCard('regex/match?', '(regex/match? #"\\d+" s)', 'Test a regex literal against a string.')}
              ${stdlibCard('file/read', '(file/read path)', 'Read a file into a string, relative to the current file.')}
              ${stdlibCard('path/join', '(path/join a b)', 'Join path segments with the platform separator.')}
            </sema-grid>
          </article>
        </sema-sidebar>
      </sema-container>
    </div>`,
};
