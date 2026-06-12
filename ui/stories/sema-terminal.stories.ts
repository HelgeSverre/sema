import type { Meta, StoryObj } from '@storybook/web-components-vite';
import '../src/lib/sema-terminal.js';

const meta: Meta = {
  title: 'Components/Terminal',
  component: 'sema-terminal',
  argTypes: {
    prompt: { control: 'text' },
    prefix: { control: 'boolean' },
    copy: { control: 'boolean' },
  },
};
export default meta;

type Story = StoryObj;

export const InstallCommand: Story = {
  render: () => `<sema-terminal style="width:520px;">$ brew install helgesverre/tap/sema-lang</sema-terminal>`,
};

export const MultiLineUsage: Story = {
  render: () =>
    `<sema-terminal style="width:560px;">
      $ sema                              # Start the REPL
      $ sema script.sema                  # Run a file
      $ sema -e '(+ 1 2)'                 # Eval expression
      $ sema -p '(filter even? (range 10))' # Eval & print
      $ sema --vm script.sema             # Use the bytecode VM
    </sema-terminal>`,
};

export const RawCommandList: Story = {
  name: 'Raw list (prefix mode, cli.html-style)',
  render: () =>
    `<sema-terminal prefix style="width:560px;">
      sema compile script.sema            # → script.semac
      sema build app.sema -o myapp        # standalone executable
      sema fmt --check
    </sema-terminal>`,
};

export const ReplSession: Story = {
  render: () =>
    `<sema-terminal prompt="sema>" style="width:420px;">
      sema> (factorial 10)
      3628800
      sema> (map inc '(1 2 3))
      (2 3 4)
    </sema-terminal>`,
};

export const WithCopy: Story = {
  render: () =>
    `<sema-terminal copy style="width:520px;">
      $ git clone https://github.com/HelgeSverre/sema
      $ cd sema && cargo build --release
    </sema-terminal>`,
};
