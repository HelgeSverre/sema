export const SEMA_COLOR_TOKEN_NAMES = [
  'bg', 'bg-elevated', 'bg-editor', 'bg-output', 'bg-toolbar',
  'gold', 'gold-dim', 'gold-glow', 'gold-soft',
  'text-primary', 'text-secondary', 'text-tertiary',
  'success', 'error', 'error-bg',
  'border', 'border-focus', 'tooltip-bg',
  'serif', 'mono',
] as const;

export const SEMA_SYNTAX_TOKEN_NAMES = [
  'syntax-comment', 'syntax-keyword', 'syntax-string', 'syntax-number',
  'syntax-boolean', 'syntax-keyword-lit', 'syntax-builtin', 'syntax-function',
  'syntax-variable', 'syntax-punctuation', 'syntax-operator', 'syntax-escape',
] as const;

export const SEMA_SPACE_TOKEN_NAMES = [
  'space-xs', 'space-sm', 'space-md', 'space-lg',
  'space-xl', 'space-2xl', 'space-3xl', 'space-4xl',
] as const;

export const SEMA_RADIUS_TOKEN_NAMES = [
  'radius-sm', 'radius-md', 'radius-lg', 'radius-xl', 'radius-pill', 'radius-full',
] as const;

export const SEMA_FOCUS_TOKEN_NAMES = [
  'focus-ring-color', 'focus-ring-color-subtle', 'focus-ring-width', 'focus-ring-offset',
] as const;

export const SEMA_CONTAINER_TOKEN_NAMES = [
  'container-prose', 'container-md', 'container-lg',
] as const;

export const SEMA_TOKEN_NAMES = [
  ...SEMA_COLOR_TOKEN_NAMES,
  ...SEMA_SYNTAX_TOKEN_NAMES,
  ...SEMA_SPACE_TOKEN_NAMES,
  ...SEMA_RADIUS_TOKEN_NAMES,
  ...SEMA_FOCUS_TOKEN_NAMES,
  ...SEMA_CONTAINER_TOKEN_NAMES,
] as const;

export type SemaTokenName = typeof SEMA_TOKEN_NAMES[number];
