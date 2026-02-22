import { defineConfig } from 'vitepress'

export default defineConfig({
  title: 'Sema',
  description: 'A Lisp with first-class LLM primitives, implemented in Rust.',

  head: [
    ['link', { rel: 'icon', type: 'image/svg+xml', href: '/favicon.svg' }],
    ['link', { rel: 'alternate', type: 'text/plain', href: '/llms.txt', title: 'LLM-friendly documentation index' }],
    ['meta', { property: 'og:type', content: 'website' }],
    ['meta', { property: 'og:title', content: 'Sema — A Lisp with LLM Primitives' }],
    ['meta', { property: 'og:description', content: 'A Scheme-like Lisp where prompts are s-expressions, conversations are persistent data structures, and LLM calls are just another form of evaluation. Implemented in Rust.' }],
    ['meta', { property: 'og:url', content: 'https://sema-lang.com' }],
    ['meta', { property: 'og:image', content: 'https://sema-lang.com/og-website.png' }],
    ['meta', { property: 'og:image:width', content: '1424' }],
    ['meta', { property: 'og:image:height', content: '752' }],
    ['meta', { property: 'og:locale', content: 'en_US' }],
    ['meta', { property: 'og:site_name', content: 'Sema' }],
    ['meta', { name: 'twitter:card', content: 'summary_large_image' }],
    ['meta', { name: 'twitter:image', content: 'https://sema-lang.com/og-website.png' }],
    ['meta', { name: 'twitter:title', content: 'Sema — A Lisp with LLM Primitives' }],
    ['meta', { name: 'twitter:description', content: 'A Scheme-like Lisp where prompts are s-expressions, conversations are persistent data structures, and LLM calls are just another form of evaluation.' }],
    ['meta', { name: 'theme-color', content: '#c8a855' }],
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
      { text: 'Playground', link: 'https://sema.run' },
    ],

    sidebar: {
      '/docs/': [
        {
          text: 'Getting Started',
          items: [
            { text: 'Introduction', link: '/docs/' },
            { text: 'CLI Reference', link: '/docs/cli' },
            { text: 'Shell Completions', link: '/docs/shell-completions' },
            { text: 'Editor Support', link: '/docs/editors' },
            { text: 'Embedding in Rust', link: '/docs/embedding' },
            { text: 'Embedding in JavaScript', link: '/docs/embedding-js' },
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
          text: 'LLM Primitives',
          items: [
            { text: 'Overview', link: '/docs/llm/' },
            { text: 'Completion & Chat', link: '/docs/llm/completion' },
            { text: 'Prompts & Messages', link: '/docs/llm/prompts' },
            { text: 'Conversations', link: '/docs/llm/conversations' },
            { text: 'Structured Extraction', link: '/docs/llm/extraction' },
            { text: 'Tools & Agents', link: '/docs/llm/tools-agents' },
            { text: 'Embeddings', link: '/docs/llm/embeddings' },
            { text: 'Vector Store & Math', link: '/docs/llm/vector-store' },
            { text: 'Caching', link: '/docs/llm/caching' },
            { text: 'Resilience & Retry', link: '/docs/llm/resilience' },
            { text: 'Providers', link: '/docs/llm/providers' },
            { text: 'Cost & Budgets', link: '/docs/llm/cost' },
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
            { text: 'PDF Processing', link: '/docs/stdlib/pdf' },
            { text: 'HTTP & JSON', link: '/docs/stdlib/http-json' },
            { text: 'Web Server', link: '/docs/stdlib/web-server' },
            { text: 'Regex', link: '/docs/stdlib/regex' },
            { text: 'CSV & Encoding', link: '/docs/stdlib/csv-encoding' },
            { text: 'Date & Time', link: '/docs/stdlib/datetime' },
            { text: 'System', link: '/docs/stdlib/system' },
            { text: 'Bytevectors', link: '/docs/stdlib/bytevectors' },
            { text: 'Records', link: '/docs/stdlib/records' },
            { text: 'Text Processing', link: '/docs/stdlib/text-processing' },
            { text: 'Key-Value Store', link: '/docs/stdlib/kv-store' },
            { text: 'Context', link: '/docs/stdlib/context' },
            { text: 'Terminal Styling', link: '/docs/stdlib/terminal' },
            { text: 'Playground & WASM', link: '/docs/stdlib/playground' },
          ],
        },
        {
          text: 'Internals',
          items: [
            { text: 'Architecture', link: '/docs/internals/architecture' },
            { text: 'Bytecode VM', link: '/docs/internals/bytecode-vm' },
            { text: 'Bytecode File Format', link: '/docs/internals/bytecode-format' },
            { text: 'Executable Format', link: '/docs/internals/executable-format' },
            { text: 'Evaluator & TCO', link: '/docs/internals/evaluator' },
            { text: 'Reader & Spans', link: '/docs/internals/reader' },
            { text: 'Performance', link: '/docs/internals/performance' },
            { text: 'Lisp Dialect Benchmark', link: '/docs/internals/lisp-comparison' },
            { text: 'Feature Comparison', link: '/docs/internals/feature-comparison' },
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
