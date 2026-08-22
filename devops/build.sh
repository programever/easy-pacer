#!/usr/bin/env bash
# Produces dist/tram-ke.html: one file, no network, open and run.
#
# That property is a hard requirement, not a convenience. The app is used on a
# mountain with no signal, so the stylesheet and the compiled Elm are inlined
# rather than linked. Vite does the inlining; the assertion below is what makes
# the requirement checked rather than merely intended.
set -euo pipefail

cd "$(dirname "$0")/.."

npx vite build
mv dist/index.html dist/tram-ke.html

# Nothing in the output may point anywhere but at itself.
if grep -oE '(src|href)="[^"]*"' dist/tram-ke.html | grep -v '="data:' ; then
  echo "build.sh: the output above references an external file; dist/tram-ke.html is not self-contained" >&2
  exit 1
fi

printf 'dist/tram-ke.html %s bytes\n' "$(wc -c < dist/tram-ke.html | tr -d ' ')"
