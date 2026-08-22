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

# Nothing in the output may point anywhere but at itself.
if grep -oE '(src|href)="[^"]*"' dist/index.html | grep -v '="data:' ; then
  echo "build.sh: the output above references an external file; dist/index.html is not self-contained" >&2
  exit 1
fi

printf 'dist/index.html %s bytes\n' "$(wc -c < dist/index.html | tr -d ' ')"
