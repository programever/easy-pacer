import { defineConfig } from 'vite'
import elm from 'vite-plugin-elm-watch'
import { viteSingleFile } from 'vite-plugin-singlefile'

// One file out. The app is used on a mountain with no signal, so the
// stylesheet and the compiled Elm are inlined rather than linked. That is a
// hard requirement, and devops/build.sh asserts it after every build.
export default defineConfig({
  // public/ holds the previous single-file build's shell. Vite would copy that
  // directory into dist verbatim; nothing there belongs in the output.
  publicDir: false,
  plugins: [elm({ mode: 'optimize' }), viteSingleFile()],
  build: {
    target: 'es2019',
    outDir: 'dist',
    assetsInlineLimit: Infinity,
  },
})
