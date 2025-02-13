#!/bin/bash

set -e  # Exit on error

# Install js_of_ocaml if not already installed
if ! command -v js_of_ocaml &> /dev/null; then
    opam install -y js_of_ocaml js_of_ocaml-ppx
fi

# Create docs directory structure
mkdir -p docs/{js,css}

# Build everything using the 1ML Makefile
cd 1ml
make clean
make
cd ..

# Create a simple development server script
cat > serve.sh << 'EOF'
#!/bin/bash
cd docs
python3 -m http.server 8000
EOF

chmod +x serve.sh

echo "Setup complete! Run ./serve.sh to start the development server" 