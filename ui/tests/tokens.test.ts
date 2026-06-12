import { describe, expect, it } from 'vitest'
import { readdirSync, readFileSync } from 'fs'
import { resolve } from 'path'

const LIB_DIR = resolve(import.meta.dirname, '../src/lib')

// sema-page.ts defines the token values themselves, so raw hex is expected there;
// index.ts is the barrel. Everything else in src/lib must use var(--token, #fallback).
const COMPONENT_FILES = readdirSync(LIB_DIR)
  .filter((file) => /^sema-.*\.ts$/.test(file) && file !== 'sema-page.ts')
  .sort()

describe('Token audit — zero hardcoded hex colors', () => {
  it('discovers component files in src/lib', () => {
    expect(COMPONENT_FILES.length).toBeGreaterThan(0)
  })

  for (const file of COMPONENT_FILES) {
    it(`${file} has no hardcoded hex colors`, () => {
      const src = readFileSync(resolve(LIB_DIR, file), 'utf-8')
      // Strip single-line and block comments
      const cleaned = src
        .replace(/\/\/.*$/gm, '')
        .replace(/\/\*[\s\S]*?\*\//g, '')
      // Remove all var() fallbacks — remaining hexes are truly hardcoded
      const withoutVars = cleaned.replace(/var\([^)]+\)/g, '')
      const hexColors = withoutVars.match(/#[0-9a-fA-F]{3,6}\b/g) || []

      if (hexColors.length > 0) {
        throw new Error(
          `Found ${hexColors.length} hardcoded hex color(s) in ${file}: ${hexColors.join(', ')}`
        )
      }
    })
  }
})
