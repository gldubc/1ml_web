# 1ML Online REPL

A web-based version of the 1ML compiler and REPL. This project allows you to write and execute 1ML code directly in your browser.

## Prerequisites

- OCaml (4.14.0 or later)
- OPAM (OCaml Package Manager)
- Python 3 (for development server)

## Setup

Run the setup script:
   ```bash
   ./setup.sh
   ```

Start the development server:
   ```bash
   ./serve.sh
   ```

Open your browser and navigate to `http://localhost:8000`

## Usage

The web interface provides a simple but powerful environment for writing and executing 1ML code:

- Write your 1ML code in the editor on the left
- Click "Run" or press Ctrl+Enter to execute the code
- See the results in the output panel on the right
- Use the "Clear" button to clear the output panel

## Acknowledgments

This uses the original 1ML implementation by Andreas Rossberg. 
