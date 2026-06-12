import type { StorybookConfig } from '@storybook/web-components-vite';

const config: StorybookConfig = {
  "stories": [
    "../stories/**/*.mdx",
    "../stories/**/*.stories.@(js|jsx|mjs|ts|tsx)"
  ],
  "addons": [
    "@storybook/addon-vitest",
    "@storybook/addon-a11y",
    "@storybook/addon-docs"
  ],
  "framework": "@storybook/web-components-vite",
  viteFinal(config) {
    config.build = { ...config.build, chunkSizeWarningLimit: 1500 };
    return config;
  },
};
export default config;