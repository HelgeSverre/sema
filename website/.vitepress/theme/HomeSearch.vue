<script setup>
// Keyboard-only local search for layout-less pages (the custom homepage uses
// `layout: false`, so VitePress's default VPNavBarSearch — which owns both the
// Cmd+K listener AND the visible search button — is never mounted). This mounts
// just the search modal and the Cmd/Ctrl+K and `/` hotkeys, with no navbar UI.
import { defineAsyncComponent, ref } from 'vue'
import { onKeyStroke } from '@vueuse/core'

// Same modal the default theme lazy-loads; pulled in on first open so the
// search payload (minisearch + index) stays off the homepage's critical path.
const VPLocalSearchBox = defineAsyncComponent(
  () => import('vitepress/dist/client/theme-default/components/VPLocalSearchBox.vue')
)

const showSearch = ref(false)

// Don't hijack `/` while the user is typing in a field (mirrors VitePress).
const isEditingContent = (event) => {
  const el = event.target
  return (
    el.isContentEditable ||
    el.tagName === 'INPUT' ||
    el.tagName === 'SELECT' ||
    el.tagName === 'TEXTAREA'
  )
}

onKeyStroke('k', (event) => {
  if (event.ctrlKey || event.metaKey) {
    event.preventDefault()
    showSearch.value = true
  }
})

onKeyStroke('/', (event) => {
  if (!isEditingContent(event)) {
    event.preventDefault()
    showSearch.value = true
  }
})
</script>

<template>
  <VPLocalSearchBox v-if="showSearch" @close="showSearch = false" />
</template>
