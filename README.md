# 1ML Online REPL

A web-based version of the 1ML compiler and REPL. Compiled from the [original 1ML implementation by Andreas Rossberg](https://github.com/rossberg/1ml) using js_of_ocaml.

This web interface provides a simple environment for writing and executing 1ML code:

- Write 1ML code in the editor on the left
- Click "Run" or press Ctrl+Enter to execute the code
- See the results in the output panel on the right
- Use the "Clear" button to clear the output panel

Try it out at: https://gldubc.github.io/1ml_web/


## To run locally:

- OCaml (4.14.0 or later)
- OPAM (OCaml Package Manager)
- Python 3 (for development server)

Run the setup script:
   ```bash
   ./setup.sh
   ```

Start the development server:
   ```bash
   ./serve.sh
   ```

Open your browser and navigate to `http://localhost:8000`
