#!/bin/bash

set -e # Exit on error

# Update elixir-modules from remote
cd elixir-modules
git fetch origin
git merge origin/master
cd ..

# Install js_of_ocaml if not already installed
if ! command -v js_of_ocaml &>/dev/null; then
  opam install -y js_of_ocaml js_of_ocaml-ppx
fi

# Create docs directory structure
mkdir -p docs/{js,css}

dune build
# Ensure target is writable if it exists
[ -f docs/js/web.js ] && chmod u+w docs/js/web.js
cp _build/default/web.bc.js docs/js/web.js

