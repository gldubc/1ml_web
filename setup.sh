#!/bin/bash

set -e # Exit on error

# Update elixir-modules from remote

# Create docs directory structure
mkdir -p docs/{js,css}

dune build
# Ensure target is writable if it exists
[ -f docs/js/web.js ] && chmod u+w docs/js/web.js
cp _build/default/web.bc.js docs/js/web.js
