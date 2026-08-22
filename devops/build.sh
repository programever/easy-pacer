#!/usr/bin/env bash
# Produces dist/index.html: one file, no network, open and run.
#
# That property is a hard requirement, not a convenience. The app is used on a
# mountain with no signal, so the stylesheet and the compiled Elm are inlined
# rather than linked. Vite does the inlining; the assertion below is what makes
# the requirement checked rather than merely intended.
#
# The file is named index.html so that the dist/ folder can be published as-is
# to GitHub Pages and answer at the site root.
set -euo pipefail

cd "$(dirname "$0")/.."

npx vite build
npx vite build --config vite.sw.config.ts

# Nothing in the app file may point anywhere but at itself. (sw.js is the one
# other file in dist/: the service worker that keeps index.html on the phone.
# It is fetched by URL and so cannot be inlined, and index.html runs without it.)
if grep -oE '(src|href)="[^"]*"' dist/index.html | grep -v '="data:' ; then
  echo "build.sh: the output above references an external file; dist/index.html is not self-contained" >&2
  exit 1
fi

printf 'dist/index.html %s bytes, dist/sw.js %s bytes\n' "$(wc -c < dist/index.html | tr -d ' ')" "$(wc -c < dist/sw.js | tr -d ' ')"
