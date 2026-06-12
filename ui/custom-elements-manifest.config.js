export default {
  litelement: true,
  globs: [
    'src/lib/*.ts',
    'src/internal/sema-element.ts',
    'src/internal/syntax-highlight.ts',
    'src/internal/toast.ts',
  ],
  exclude: ['**/*.{test,spec,stories}.{ts,js}', '**/index.ts'],
  outdir: '.',
};
