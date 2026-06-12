<script setup>
import { ref } from 'vue'

const copied = ref({})

const copyToClipboard = (text, key) => {
  if (navigator.clipboard) {
    navigator.clipboard.writeText(text).then(() => {
      copied.value[key] = true
      setTimeout(() => {
        copied.value[key] = false
      }, 2000)
    }).catch(err => {
      console.error('Failed to copy text: ', err)
    })
  }
}

const colors = [
  { name: 'gold', hex: '#c8a855', role: 'Primary accent, CTA, links, active states, key keywords.', type: 'Accent' },
  { name: 'gold-dim', hex: 'rgba(200, 168, 85, 0.5)', role: 'Transparent borders, stale/inactive indicators.', type: 'Accent Opacity' },
  { name: 'gold-glow', hex: 'rgba(200, 168, 85, 0.08)', role: 'Hover backgrounds on interactive rows.', type: 'Accent Opacity' },
  { name: 'gold-soft', hex: 'rgba(200, 168, 85, 0.14)', role: 'Selection overlay background.', type: 'Accent Opacity' },
  { name: 'bg', hex: '#131110', role: 'Page canvas, sidebar background, main window.', type: 'Background' },
  { name: 'bg-elevated', hex: '#181512', role: 'Cards, toolbars, raised surfaces, modals.', type: 'Background' },
  { name: 'bg-editor', hex: '#1c1916', role: 'Code editing areas, text inputs, focused blocks.', type: 'Background' },
  { name: 'bg-output', hex: '#0f0d0c', role: 'Deepest level. Output panels, read-only consoles.', type: 'Background' },
  { name: 'text-primary', hex: '#e9e3d6', role: 'Body copy on marketing pages, primary headings.', type: 'Text' },
  { name: 'text-secondary', hex: '#968c79', role: 'Standard UI labels, secondary headings, editor text.', type: 'Text' },
  { name: 'text-tertiary', hex: '#6b6354', role: 'Muted comments, captions, disabled states.', type: 'Text' },
  { name: 'border', hex: '#2b2620', role: 'Default borders, dividers, subtle structure.', type: 'Structure' },
  { name: 'border-focus', hex: '#c8a855', role: 'Highlighted/focused borders.', type: 'Structure' },
  { name: 'success', hex: '#6a9955', role: 'Success indicators, green highlights.', type: 'Semantic' },
  { name: 'error', hex: '#c85555', role: 'Error messages, validation alerts.', type: 'Semantic' }
]

const logoSvgCode = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 366.00 132.00" width="366.00" height="132.00">
  <!-- Parentheses in Gold (#c8a855), Text in White (#ffffff) -->
  <path d="M48.5000 104.3000L48.5000 114Q34 110.7000 26.0500 100.5000Q18.1000 90.3000 18.1000 75L18.1000 57Q18.1000 41.7000 26.0500 31.5000Q34 21.3000 48.5000 18L48.5000 27.6000Q42.2000 29.1000 37.6000 33.1500Q33 37.2000 30.5000 43.3000Q28 49.4000 28 57L28 75Q28 82.6000 30.5000 88.6500Q33 94.7000 37.6000 98.7500Q42.2000 102.8000 48.5000 104.3000" fill="#c8a855" />
  <path d="M93.2000 102.8000L88.8000 102.8000Q79.4000 102.8000 74.2000 98.6000Q69 94.4000 69 86.8000L78.8000 86.8000Q78.8000 90.4000 81.4500 92.4500Q84.1000 94.5000 88.8000 94.5000L93.2000 94.5000Q98.1000 94.5000 100.7500 92.4000Q103.4000 90.3000 103.4000 86.5000Q103.4000 79.8000 96.8000 79L82 76.9000Q76.1000 76 72.9000 72.0500Q69.7000 68.1000 69.7000 61.8000Q69.7000 54.4000 74.7000 50.3000Q79.7000 46.2000 88.7000 46.2000L93.1000 46.2000Q101.5000 46.2000 106.7000 50.2000Q111.9000 54.2000 112.2000 60.8000L102.2000 60.8000Q102 58 99.6000 56.1500Q97.2000 54.3000 93.1000 54.3000L88.7000 54.3000Q84.2000 54.3000 81.7500 56.3000Q79.3000 58.3000 79.3000 61.7000Q79.3000 67.2000 84.8000 67.9000L98.7000 69.9000Q113 71.8000 113 86.5000Q113 94.3000 107.8500 98.5500Q102.7000 102.8000 93.2000 102.8000 M152 103Q142.1000 103 136.0500 97.1000Q130 91.2000 130 81L130 68Q130 57.8000 136.0500 51.9000Q142.1000 46 152 46Q158.6000 46 163.5500 48.6500Q168.5000 51.3000 171.2500 56.0500Q174 60.8000 174 67.1000L174 77L139.7000 77L139.7000 81.8000Q139.7000 87.8000 143 91.2000Q146.3000 94.6000 152 94.6000Q156.8000 94.6000 159.9000 92.8000Q163 91 163.6000 87.8000L173.5000 87.8000Q172.5000 94.8000 166.6000 98.9000Q160.7000 103 152 103M139.7000 67.1000L139.7000 69.7000L164.3000 69.7000L164.3000 67.1000Q164.3000 60.8000 161.1000 57.4000Q157.9000 54 152 54Q146.1000 54 142.9000 57.4000Q139.7000 60.8000 139.7000 67.1000 M197.7000 102L188.7000 102L188.7000 47L197.1000 47L197.1000 54.5000L197.4000 54.5000Q197.8000 50.7000 200.2500 48.3500Q202.7000 46 206.5000 46Q210.2000 46 212.7000 48.2000Q215.2000 50.4000 216.3000 54.1000Q216.9000 50.3000 219.4000 48.1500Q221.9000 46 225.7000 46Q230.9000 46 234.1000 49.9500Q237.3000 53.9000 237.3000 60.2000L237.3000 102L228.3000 102L228.3000 60.3000Q228.3000 57.2000 226.7500 55.3500Q225.2000 53.5000 222.6000 53.5000Q220 53.5000 218.5000 55.3000Q217 57.1000 217 60.3000L217 102L209 102L209 60.3000Q209 57.2000 207.5000 55.3500Q206 53.5000 203.4000 53.5000Q200.8000 53.5000 199.2500 55.3000Q197.7000 57.1000 197.7000 60.3000 M268.9000 103Q260.4000 103 255.4500 98.2500Q250.5000 93.5000 250.5000 85.8000Q250.5000 78.1000 255.6500 73.4000Q260.8000 68.7000 269.2000 68.7000L285.3000 68.7000L285.3000 64.5000Q285.3000 54.4000 274.1000 54.4000Q269.1000 54.4000 266.0500 56.2500Q263 58.1000 262.8000 61.4000L253 61.4000Q253.5000 54.7000 259.1000 50.3500Q264.7000 46 274.1000 46Q284.2000 46 289.7000 50.8000Q295.2000 55.6000 295.2000 64.3000L295.2000 102L285.5000 102L285.5000 91.9000L285.3000 91.9000Q284.6000 97 280.2500 100Q275.9000 103 268.9000 103M271.5000 94.7000Q277.8000 94.7000 281.5500 91.6500Q285.3000 88.6000 285.3000 83.3000L285.3000 76L270.1000 76Q265.8000 76 263.1500 78.5500Q260.5000 81.1000 260.5000 85.3000Q260.5000 89.6000 263.4000 92.1500Q266.3000 94.7000 271.5000 94.7000" fill="#ffffff" />
  <path d="M316.5000 114L316.5000 104.3000Q322.8000 102.8000 327.4000 98.7500Q332 94.7000 334.5500 88.6500Q337.1000 82.6000 337.1000 75L337.1000 57Q337.1000 49.4000 334.5500 43.3000Q332 37.2000 327.4000 33.1500Q322.8000 29.1000 316.5000 27.6000L316.5000 18Q331 21.3000 339 31.5000Q347 41.7000 347 57L347 75Q347 90.3000 339 100.5000Q331 110.7000 316.5000 114" fill="#c8a855" />
</svg>`
</script>

<template>
  <div class="brand-guide">
    <!-- Hero Header -->
    <header class="brand-hero">
      <div class="brand-container">
        <span class="hero-tag">System Design</span>
        <h1 class="hero-title">Sema Brand &amp; Style Guide</h1>
        <p class="hero-subtitle">
          Visual grammar for the Sema Lisp brand identity. Built around warm architectural dark tones, a single gold accent, and absolute font boundaries.
        </p>
      </div>
    </header>

    <div class="brand-container">
      <!-- Logo Section -->
      <section class="brand-section">
        <div class="section-meta">
          <span class="section-num">01</span>
          <h2 class="section-title">The Logo Mark</h2>
          <p class="section-desc">
            The canonical logotype <code>(sema)</code> combines parentheses in warm gold with high-contrast monospace letters in pure white. It represents the marriage of Lisp s-expressions and modern machine-learning models.
          </p>
        </div>

        <div class="logo-display-card">
          <div class="logo-preview-area">
            <!-- Inline Rendered SVG -->
            <svg class="preview-svg" viewBox="0 0 366 132" width="366" height="132" fill="none">
              <path d="M48.5000 104.3000L48.5000 114Q34 110.7000 26.0500 100.5000Q18.1000 90.3000 18.1000 75L18.1000 57Q18.1000 41.7000 26.0500 31.5000Q34 21.3000 48.5000 18L48.5000 27.6000Q42.2000 29.1000 37.6000 33.1500Q33 37.2000 30.5000 43.3000Q28 49.4000 28 57L28 75Q28 82.6000 30.5000 88.6500Q33 94.7000 37.6000 98.7500Q42.2000 102.8000 48.5000 104.3000" fill="#c8a855" />
              <path d="M93.2000 102.8000L88.8000 102.8000Q79.4000 102.8000 74.2000 98.6000Q69 94.4000 69 86.8000L78.8000 86.8000Q78.8000 90.4000 81.4500 92.4500Q84.1000 94.5000 88.8000 94.5000L93.2000 94.5000Q98.1000 94.5000 100.7500 92.4000Q103.4000 90.3000 103.4000 86.5000Q103.4000 79.8000 96.8000 79L82 76.9000Q76.1000 76 72.9000 72.0500Q69.7000 68.1000 69.7000 61.8000Q69.7000 54.4000 74.7000 50.3000Q79.7000 46.2000 88.7000 46.2000L93.1000 46.2000Q101.5000 46.2000 106.7000 50.2000Q111.9000 54.2000 112.2000 60.8000L102.2000 60.8000Q102 58 99.6000 56.1500Q97.2000 54.3000 93.1000 54.3000L88.7000 54.3000Q84.2000 54.3000 81.7500 56.3000Q79.3000 58.3000 79.3000 61.7000Q79.3000 67.2000 84.8000 67.9000L98.7000 69.9000Q113 71.8000 113 86.5000Q113 94.3000 107.8500 98.5500Q102.7000 102.8000 93.2000 102.8000 M152 103Q142.1000 103 136.0500 97.1000Q130 91.2000 130 81L130 68Q130 57.8000 136.0500 51.9000Q142.1000 46 152 46Q158.6000 46 163.5500 48.6500Q168.5000 51.3000 171.2500 56.0500Q174 60.8000 174 67.1000L174 77L139.7000 77L139.7000 81.8000Q139.7000 87.8000 143 91.2000Q146.3000 94.6000 152 94.6000Q156.8000 94.6000 159.9000 92.8000Q163 91 163.6000 87.8000L173.5000 87.8000Q172.5000 94.8000 166.6000 98.9000Q160.7000 103 152 103M139.7000 67.1000L139.7000 69.7000L164.3000 69.7000L164.3000 67.1000Q164.3000 60.8000 161.1000 57.4000Q157.9000 54 152 54Q146.1000 54 142.9000 57.4000Q139.7000 60.8000 139.7000 67.1000 M197.7000 102L188.7000 102L188.7000 47L197.1000 47L197.1000 54.5000L197.4000 54.5000Q197.8000 50.7000 200.2500 48.3500Q202.7000 46 206.5000 46Q210.2000 46 212.7000 48.2000Q215.2000 50.4000 216.3000 54.1000Q216.9000 50.3000 219.4000 48.1500Q221.9000 46 225.7000 46Q230.9000 46 234.1000 49.9500Q237.3000 53.9000 237.3000 60.2000L237.3000 102L228.3000 102L228.3000 60.3000Q228.3000 57.2000 226.7500 55.3500Q225.2000 53.5000 222.6000 53.5000Q220 53.5000 218.5000 55.3000Q217 57.1000 217 60.3000L217 102L209 102L209 60.3000Q209 57.2000 207.5000 55.3500Q206 53.5000 203.4000 53.5000Q200.8000 53.5000 199.2500 55.3000Q197.7000 57.1000 197.7000 60.3000 M268.9000 103Q260.4000 103 255.4500 98.2500Q250.5000 93.5000 250.5000 85.8000Q250.5000 78.1000 255.6500 73.4000Q260.8000 68.7000 269.2000 68.7000L285.3000 68.7000L285.3000 64.5000Q285.3000 54.4000 274.1000 54.4000Q269.1000 54.4000 266.0500 56.2500Q263 58.1000 262.8000 61.4000L253 61.4000Q253.5000 54.7000 259.1000 50.3500Q264.7000 46 274.1000 46Q284.2000 46 289.7000 50.8000Q295.2000 55.6000 295.2000 64.3000L295.2000 102L285.5000 102L285.5000 91.9000L285.3000 91.9000Q284.6000 97 280.2500 100Q275.9000 103 268.9000 103M271.5000 94.7000Q277.8000 94.7000 281.5500 91.6500Q285.3000 88.6000 285.3000 83.3000L285.3000 76L270.1000 76Q265.8000 76 263.1500 78.5500Q260.5000 81.1000 260.5000 85.3000Q260.5000 89.6000 263.4000 92.1500Q266.3000 94.7000 271.5000 94.7000" fill="#ffffff" />
              <path d="M316.5000 114L316.5000 104.3000Q322.8000 102.8000 327.4000 98.7500Q332 94.7000 334.5500 88.6500Q337.1000 82.6000 337.1000 75L337.1000 57Q337.1000 49.4000 334.5500 43.3000Q332 37.2000 327.4000 33.1500Q322.8000 29.1000 316.5000 27.6000L316.5000 18Q331 21.3000 339 31.5000Q347 41.7000 347 57L347 75Q347 90.3000 339 100.5000Q331 110.7000 316.5000 114" fill="#c8a855" />
            </svg>
          </div>
          <div class="logo-code-area">
            <div class="code-header">
              <span>logo.svg</span>
              <button class="btn-copy-code" @click="copyToClipboard(logoSvgCode, 'svg')">
                {{ copied['svg'] ? 'Copied!' : 'Copy SVG Vector' }}
              </button>
            </div>
            <pre class="code-view"><code>{{ logoSvgCode }}</code></pre>
          </div>
        </div>
      </section>

      <!-- Color Palette Section -->
      <section class="brand-section">
        <div class="section-meta">
          <span class="section-num">02</span>
          <h2 class="section-title">Color Palette</h2>
          <p class="section-desc">
            Sema relies on a strict dark-only architectural hierarchy. No pure blacks, no pure whites. Colors are classified by background depth, accents, and semantic output feedback. Click any swatch value to copy it.
          </p>
        </div>

        <div class="palette-grid">
          <div 
            v-for="color in colors" 
            :key="color.name" 
            class="color-card"
            @click="copyToClipboard(color.hex, color.name)"
          >
            <div class="color-preview" :style="{ backgroundColor: color.hex }">
              <!-- Overlay grid lines for depth visualization -->
              <div class="preview-depth-lines"></div>
            </div>
            <div class="color-details">
              <div class="color-card-header">
                <span class="color-type">{{ color.type }}</span>
                <span class="copy-indicator" :class="{ 'is-copied': copied[color.name] }">
                  {{ copied[color.name] ? 'Copied!' : 'Copy Value' }}
                </span>
              </div>
              <h3 class="color-name">--{{ color.name }}</h3>
              <p class="color-hex">{{ color.hex }}</p>
              <p class="color-role">{{ color.role }}</p>
            </div>
          </div>
        </div>
      </section>

      <!-- Typography Section -->
      <section class="brand-section">
        <div class="section-meta">
          <span class="section-num">03</span>
          <h2 class="section-title">Typography &amp; Specimen</h2>
          <p class="section-desc">
            The typography operates under an absolute dual-font regime. <strong>Cormorant Serif</strong> conveys historical editorial gravity for prose and headings. <strong>JetBrains Mono</strong> handles technical controls, code fields, and syntax tokens. Inter acts as clean layout body text.
          </p>
        </div>

        <div class="type-container">
          <!-- Font 1 -->
          <div class="font-specimen-block">
            <h3 class="font-label">Cormorant — Editorial &amp; Headings</h3>
            <div class="font-preview-large serif-font">
              Aa Bb Cc Dd Ee Ff Gg Hh Ii Jj Kk Ll Mm Nn Oo Pp Qq Rr Ss Tt Uu Vv Ww Xx Yy Zz
            </div>
            
            <div class="font-specimen-lines">
              <div class="specimen-line">
                <div class="specimen-info">Heading 1 · 300 Light</div>
                <div class="specimen-sample serif-font h1-preview">The future of programming is conversational</div>
              </div>
              <div class="specimen-line">
                <div class="specimen-info">Heading 2 · 300 Light</div>
                <div class="specimen-sample serif-font h2-preview">Evaluation as a dialogical loop</div>
              </div>
              <div class="specimen-line">
                <div class="specimen-info">Heading 3 · 500 Medium</div>
                <div class="specimen-sample serif-font h3-preview">Special Forms &amp; Native Evaluator</div>
              </div>
            </div>
          </div>

          <!-- Font 2 -->
          <div class="font-specimen-block">
            <h3 class="font-label">JetBrains Mono — Syntax &amp; Code</h3>
            <div class="font-preview-large mono-font">
              Aa Bb Cc Dd Ee Ff Gg Hh Ii Jj Kk Ll Mm Nn Oo Pp Qq Rr Ss Tt Uu Vv Ww Xx Yy Zz 1234567890
            </div>

            <div class="font-specimen-lines">
              <div class="specimen-line">
                <div class="specimen-info">Code Block · 400 Regular</div>
                <div class="specimen-sample mono-font code-preview">
                  (defagent coder {:model "claude-haiku-4-5" :system "Rust programmer"})
                </div>
              </div>
              <div class="specimen-line">
                <div class="specimen-info">Code Small · 400 Regular</div>
                <div class="specimen-sample mono-font code-preview-small">
                  (map (lambda (x) (* x 2)) (range 1 10)) ; => (2 4 6 8 10 12 14 16 18)
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>

      <!-- Design Rules -->
      <section class="brand-section last-section">
        <div class="section-meta">
          <span class="section-num">04</span>
          <h2 class="section-title">Visual Architecture Rules</h2>
          <p class="section-desc">
            Visual consistency rules governing the construction of any Sema user interface wrapper or documentation page.
          </p>
        </div>

        <div class="rules-grid">
          <div class="rule-card">
            <h4>No Drop Shadows</h4>
            <p>Depth is represented entirely through color elevation: surfaces closer to the user are lighter warm-neutrals (e.g. <code>#181512</code>), while background layers are deeper/darker (e.g. <code>#131110</code>).</p>
          </div>
          <div class="rule-card">
            <h4>Absolute Typography boundaries</h4>
            <p>Monospace text is only used for executable s-expressions, CLI snippets, interactive button labels, and system status tags. Normal text/descriptions must use a sans-serif typeface.</p>
          </div>
          <div class="rule-card">
            <h4>Gold is a Precision Tool</h4>
            <p>Gold (<code>#c8a855</code>) is reserved for highlighting primary active paths (hover states, focus borders, primary action triggers). Do not scatter gold decorative borders across structural containers.</p>
          </div>
          <div class="rule-card">
            <h4>Border Thickness</h4>
            <p>Default borders must be exactly <code>1px</code> solid with color <code>#2b2620</code>. Focus borders or active left margins use <code>2px</code> or <code>3px</code> solid gold.</p>
          </div>
        </div>
      </section>
    </div>
  </div>
</template>

<style scoped>
.brand-guide {
  background-color: #131110;
  color: #968c79;
  font-family: 'Inter', system-ui, sans-serif;
  min-height: 100vh;
  padding-bottom: 6rem;
  line-height: 1.6;
  -webkit-font-smoothing: antialiased;
}

.brand-container {
  max-width: 1100px;
  margin: 0 auto;
  padding: 0 2rem;
}

/* Hero Section */
.brand-hero {
  padding: 6rem 0 4rem;
  border-bottom: 1px solid #2b2620;
  background: radial-gradient(ellipse at 50% 100%, rgba(200, 168, 85, 0.04), transparent 70%);
  text-align: center;
}

.hero-tag {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  text-transform: uppercase;
  color: #c8a855;
  letter-spacing: 0.15em;
  display: inline-block;
  margin-bottom: 1rem;
  border: 1px solid rgba(200, 168, 85, 0.2);
  padding: 0.25rem 0.75rem;
  border-radius: 20px;
  background: rgba(200, 168, 85, 0.02);
}

.hero-title {
  font-family: 'Cormorant', Georgia, serif;
  font-size: clamp(2.5rem, 5vw, 4rem);
  font-weight: 300;
  color: #e9e3d6;
  line-height: 1.15;
  margin-bottom: 1.5rem;
  letter-spacing: -0.01em;
}

.hero-subtitle {
  font-size: 1.15rem;
  color: #968c79;
  max-width: 44rem;
  margin: 0 auto;
  font-weight: 300;
}

/* Section Layout */
.brand-section {
  padding: 5rem 0;
  border-bottom: 1px solid #2b2620;
}

.brand-section.last-section {
  border-bottom: none;
}

.section-meta {
  margin-bottom: 3rem;
  max-width: 44rem;
}

.section-num {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.8rem;
  color: #c8a855;
  display: block;
  margin-bottom: 0.5rem;
  opacity: 0.8;
}

.section-title {
  font-family: 'Cormorant', Georgia, serif;
  font-size: 2.2rem;
  font-weight: 300;
  color: #e9e3d6;
  margin-bottom: 1rem;
}

.section-desc {
  font-size: 1.05rem;
  color: #968c79;
}

/* Logo Showcase */
.logo-display-card {
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
  background-color: #181512;
  border: 1px solid #2b2620;
  border-radius: 8px;
  overflow: hidden;
}

.logo-preview-area {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 4rem;
  background: radial-gradient(circle, rgba(200, 168, 85, 0.05) 0%, transparent 80%), #1c1916;
  border-right: 1px solid #2b2620;
  position: relative;
}

.logo-preview-area::after {
  content: '';
  position: absolute;
  inset: 0;
  background-image: radial-gradient(#2b2620 1px, transparent 1px);
  background-size: 16px 16px;
  opacity: 0.25;
  pointer-events: none;
}

.preview-svg {
  width: 100%;
  max-width: 280px;
  height: auto;
  filter: drop-shadow(0 4px 24px rgba(0,0,0,0.25));
  z-index: 1;
}

.logo-code-area {
  display: flex;
  flex-direction: column;
  height: 100%;
  min-width: 0;
}

.code-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.75rem 1.25rem;
  background-color: #181512;
  border-bottom: 1px solid #2b2620;
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  color: #6b6354;
}

.btn-copy-code {
  background-color: transparent;
  border: 1px solid #2b2620;
  color: #c8a855;
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  cursor: pointer;
  transition: all 0.2s;
  font-size: 0.7rem;
}

.btn-copy-code:hover {
  background-color: rgba(200, 168, 85, 0.08);
  border-color: #c8a855;
}

.code-view {
  margin: 0;
  padding: 1.25rem;
  background-color: #0f0d0c;
  overflow: auto;
  flex-grow: 1;
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  line-height: 1.6;
  color: #968c79;
  max-height: 250px;
}

/* Color Cards */
.palette-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 1.5rem;
}

.color-card {
  background-color: #181512;
  border: 1px solid #2b2620;
  border-radius: 6px;
  overflow: hidden;
  cursor: pointer;
  transition: transform 0.2s, border-color 0.2s, box-shadow 0.2s;
}

.color-card:hover {
  transform: translateY(-2px);
  border-color: rgba(200, 168, 85, 0.4);
  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
}

.color-preview {
  height: 90px;
  position: relative;
}

.preview-depth-lines {
  position: absolute;
  inset: 0;
  background: repeating-linear-gradient(
    45deg,
    transparent,
    transparent 6px,
    rgba(255, 255, 255, 0.01) 6px,
    rgba(255, 255, 255, 0.01) 12px
  );
}

.color-details {
  padding: 1.25rem;
}

.color-card-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.25rem;
}

.color-type {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.65rem;
  text-transform: uppercase;
  color: #6b6354;
  letter-spacing: 0.05em;
}

.copy-indicator {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.65rem;
  color: #6b6354;
  opacity: 0;
  transition: opacity 0.2s, color 0.2s;
}

.color-card:hover .copy-indicator {
  opacity: 1;
}

.copy-indicator.is-copied {
  opacity: 1 !important;
  color: #c8a855;
  font-weight: 500;
}

.color-name {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.9rem;
  color: #e9e3d6;
  margin: 0 0 0.25rem 0;
  font-weight: 500;
}

.color-hex {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  color: #c8a855;
  margin: 0 0 0.75rem 0;
}

.color-role {
  font-size: 0.8rem;
  color: #968c79;
  line-height: 1.5;
  margin: 0;
  font-style: italic;
  font-family: 'Cormorant', Georgia, serif;
}

/* Typography Specimens */
.type-container {
  display: flex;
  flex-direction: column;
  gap: 3rem;
}

.font-specimen-block {
  background-color: #181512;
  border: 1px solid #2b2620;
  border-radius: 8px;
  padding: 2.5rem;
}

.font-label {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  color: #c8a855;
  text-transform: uppercase;
  letter-spacing: 0.1em;
  margin-bottom: 1.5rem;
  border-bottom: 1px solid #2b2620;
  padding-bottom: 0.75rem;
}

.font-preview-large {
  font-size: 1.4rem;
  line-height: 1.5;
  color: #e9e3d6;
  padding-bottom: 2rem;
  border-bottom: 1px dashed #2b2620;
  margin-bottom: 2rem;
}

.serif-font {
  font-family: 'Cormorant', Georgia, serif;
}

.mono-font {
  font-family: 'JetBrains Mono', monospace;
}

.font-specimen-lines {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}

.specimen-line {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.specimen-info {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.65rem;
  color: #6b6354;
  text-transform: uppercase;
}

.specimen-sample {
  color: #e9e3d6;
  line-height: 1.3;
}

.h1-preview {
  font-size: 2.2rem;
  font-weight: 300;
}

.h2-preview {
  font-size: 1.8rem;
  font-weight: 300;
}

.h3-preview {
  font-size: 1.4rem;
  font-weight: 500;
}

.code-preview {
  font-size: 0.85rem;
  background-color: #0f0d0c;
  padding: 0.75rem 1rem;
  border-radius: 4px;
  border: 1px solid #2b2620;
}

.code-preview-small {
  font-size: 0.75rem;
  background-color: #0f0d0c;
  padding: 0.75rem 1rem;
  border-radius: 4px;
  border: 1px solid #2b2620;
}

/* Rules Grid */
.rules-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: 1.5rem;
}

.rule-card {
  background-color: #181512;
  border: 1px solid #2b2620;
  border-radius: 6px;
  padding: 1.5rem;
}

.rule-card h4 {
  font-family: 'Cormorant', Georgia, serif;
  font-size: 1.25rem;
  color: #e9e3d6;
  margin: 0 0 0.75rem 0;
  font-weight: 500;
}

.rule-card p {
  font-size: 0.85rem;
  color: #968c79;
  line-height: 1.5;
  margin: 0;
}

@media (max-width: 768px) {
  .logo-display-card {
    grid-template-columns: 1fr;
  }
  .logo-preview-area {
    border-right: none;
    border-bottom: 1px solid #2b2620;
    padding: 3rem;
  }
  .brand-hero {
    padding: 4rem 0 3rem;
  }
  .font-specimen-block {
    padding: 1.5rem;
  }
}
</style>
