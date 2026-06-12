import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-code.js';
import { SemaCode } from '../src/lib/sema-code.js';

const meta: Meta = {
  title: 'Components/Code',
  component: 'sema-code',
  argTypes: {
    lang: { control: 'text' },
    'no-dedent': { control: 'boolean' },
    'no-highlight': { control: 'boolean' },
    format: { control: 'boolean' },
    copy: { control: 'boolean' },
    lines: { control: 'boolean' },
  },
};
export default meta;

type Story = StoryObj;

// A snippet authored deeply indented inside the markup — dedent makes it render flush.
const SEMA_SNIPPET = `
      ;; Define a tool the LLM can call
      (deftool get-weather
        "Get weather for a city"
        {:city {:type :string}}
        (lambda (city)
          (format "~a: 22°C, sunny" city)))

      (agent/run weather-bot
        "What's the weather in Tokyo?")
`;

export const SemaHighlighted: Story = {
  render: () => `<sema-code>${SEMA_SNIPPET}</sema-code>`,
};

export const Plain: Story = {
  name: 'Plain (no-highlight)',
  render: () => `<sema-code no-highlight>${SEMA_SNIPPET}</sema-code>`,
};

export const DedentDemo: Story = {
  name: 'Dedent (indented source → flush)',
  render: () =>
    `<sema-code>
                    (define (square x)
                      (* x x))
                    (square 9)
    </sema-code>`,
};

export const NoDedent: Story = {
  name: 'no-dedent (preserve indentation)',
  render: () => `<sema-code no-dedent>${SEMA_SNIPPET}</sema-code>`,
};

export const WithLineNumbers: Story = {
  render: () => `<sema-code lines>${SEMA_SNIPPET}</sema-code>`,
};

export const WithCopyButton: Story = {
  render: () => `<sema-code copy>${SEMA_SNIPPET}</sema-code>`,
};

export const UnknownLanguage: Story = {
  name: 'Unknown lang (plain, no grammar)',
  render: () => `<sema-code lang="json">{ "model": "claude-haiku-4-5", "tokens": 22 }</sema-code>`,
};

export const Formatted: Story = {
  name: 'Formatted (mock formatter)',
  render: () => {
    // Demo: register a trivial formatter that collapses runs of spaces.
    SemaCode.formatter = (code) => code.replace(/[ \t]{2,}/g, ' ');
    return `<sema-code format>(  define    x     42  )</sema-code>`;
  },
};
