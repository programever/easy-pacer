import { defineConfig } from 'vite'

// The service worker is the one piece that cannot be inlined: the browser
// must fetch it by URL, at a stable name, from the same origin as the page.
// So it is built on its own, after the app, into the same dist/ folder.
export default defineConfig({
  publicDir: false,
  build: {
    outDir: 'dist',
    emptyOutDir: false,
    target: 'es2019',
    lib: {
      entry: 'runtime/sw.ts',
      formats: ['iife'],
      name: 'sw',
      fileName: () => 'sw.js',
    },
  },
})
