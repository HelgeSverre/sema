import eslint from '@eslint/js';
import tseslint from 'typescript-eslint';
import litPlugin from 'eslint-plugin-lit';
import litA11yPlugin from 'eslint-plugin-lit-a11y';
import wcPlugin from 'eslint-plugin-wc';
import storybookPlugin from 'eslint-plugin-storybook';

export default tseslint.config(
  eslint.configs.recommended,
  tseslint.configs.recommended,
  litPlugin.configs['flat/recommended'],
  {
    plugins: { 'lit-a11y': litA11yPlugin },
    rules: litA11yPlugin.configs.recommended.rules,
  },
  wcPlugin.configs['flat/recommended'],
  ...storybookPlugin.configs['flat/recommended'],
  {
    settings: {
      wc: {
        elementBaseClasses: ['LitElement', 'SemaElement'],
      },
    },
    rules: {
      '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: '^_' }],
    },
  },
  {
    ignores: ['dist/', 'node_modules/', '.storybook/'],
  }
);
