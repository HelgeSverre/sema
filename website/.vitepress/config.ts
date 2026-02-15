import { defineConfig } from 'vitepress'

export default defineConfig({
  title: 'Sema',
  description: 'A Lisp with first-class LLM primitives, implemented in Rust.',

  head: [
    ['link', { rel: 'icon', type: 'image/svg+xml', href: '/favicon.svg' }],
    ['link', { rel: 'preconnect', href: 'https://fonts.googleapis.com' }],
    ['link', { rel: 'preconnect', href: 'https://fonts.gstatic.com', crossorigin: '' }],
    ['link', { href: 'https://fonts.googleapis.com/css2?family=Cormorant:ital,wght@0,300;0,400;0,500;0,600;1,400&family=JetBrains+Mono:wght@400;500&display=swap', rel: 'stylesheet' }],
  ],

  appearance: 'dark',

  themeConfig: {
    logo: undefined,
    siteTitle: 'Sema',

    nav: [
      { text: 'Home', link: '/' },
      { text: 'Docs', link: '/docs/' },
      { text: 'Stdlib', link: '/docs/stdlib/' },
      { text: 'LLM', link: '/docs/llm/' },
    ],

    sidebar: {
      '/docs/': [
        {
          text: 'Getting Started',
          items: [
            { text: 'Introduction', link: '/docs/' },
            { text: 'CLI Reference', link: '/docs/cli' },
            { text: 'Embedding in Rust', link: '/docs/embedding' },
          ],
        },
        {
          text: 'Language Reference',
          items: [
            { text: 'Data Types', link: '/docs/language/data-types' },
            { text: 'Special Forms', link: '/docs/language/special-forms' },
            { text: 'Macros & Modules', link: '/docs/language/macros-modules' },
          ],
        },
        {
          text: 'Standard Library',
          items: [
            { text: 'Overview', link: '/docs/stdlib/' },
            { text: 'Math & Arithmetic', link: '/docs/stdlib/math' },
            { text: 'Strings & Characters', link: '/docs/stdlib/strings' },
            { text: 'Lists', link: '/docs/stdlib/lists' },
            { text: 'Vectors', link: '/docs/stdlib/vectors' },
            { text: 'Maps & HashMaps', link: '/docs/stdlib/maps' },
            { text: 'Predicates', link: '/docs/stdlib/predicates' },
            { text: 'File I/O & Paths', link: '/docs/stdlib/file-io' },
            { text: 'HTTP & JSON', link: '/docs/stdlib/http-json' },
            { text: 'Regex', link: '/docs/stdlib/regex' },
            { text: 'CSV & Encoding', link: '/docs/stdlib/csv-encoding' },
            { text: 'Date & Time', link: '/docs/stdlib/datetime' },
            { text: 'System', link: '/docs/stdlib/system' },
            { text: 'Bytevectors', link: '/docs/stdlib/bytevectors' },
            { text: 'Records', link: '/docs/stdlib/records' },
          ],
        },
        {
          text: 'Internals',
          items: [
            { text: 'Architecture', link: '/docs/internals/architecture' },
            { text: 'Evaluator & TCO', link: '/docs/internals/evaluator' },
            { text: 'Reader & Spans', link: '/docs/internals/reader' },
            { text: 'Performance', link: '/docs/internals/performance' },
            { text: 'Lisp Dialect Benchmark', link: '/docs/internals/lisp-comparison' },
          ],
        },
        {
          text: 'LLM Primitives',
          items: [
            { text: 'Overview', link: '/docs/llm/' },
            { text: 'Completion & Chat', link: '/docs/llm/completion' },
            { text: 'Prompts & Messages', link: '/docs/llm/prompts' },
            { text: 'Conversations', link: '/docs/llm/conversations' },
            { text: 'Structured Extraction', link: '/docs/llm/extraction' },
            { text: 'Tools & Agents', link: '/docs/llm/tools-agents' },
            { text: 'Embeddings', link: '/docs/llm/embeddings' },
            { text: 'Providers', link: '/docs/llm/providers' },
            { text: 'Cost & Budgets', link: '/docs/llm/cost' },
          ],
        },
      ],
    },

    search: {
      provider: 'local',
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/HelgeSverre/sema' },
    ],

    outline: { level: [2, 3] },
  },

  srcExclude: ['**/node_modules/**'],
})
