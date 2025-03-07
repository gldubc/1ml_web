# 1ML Online REPL

A web-based version of the 1ML compiler and REPL, built using [Andreas Rossberg’s original 1ML implementation](https://github.com/rossberg/1ml) and compiled to JavaScript with js_of_ocaml.

This web interface provides a simple environment for writing and executing 1ML code:

- Write 1ML code in the editor on the left
- Click **"Run"** or press **Ctrl+Enter** to execute.
- View the results in the output panel on the right.
- Use **"Clear"** to reset the output

Try it out here: https://gldubc.github.io/1ml_web/

## Project Structure

The project is organized as follows:

- `src/`: Contains the source code for the web interface
- `build/`: Contains build artifacts (created during build)
- `docs/`: Contains the deployable web application
  - `docs/js/`: Contains the compiled JavaScript (web.js)
  - `docs/index.html`: The main HTML file for the web application
- `1ml/`: The 1ML compiler 
- `elixir-modules/`: Elixir modules dependency

## Dependencies

- 1ML compiler (included in the 1ml/ directory)
- Elixir modules (included in the elixir-modules/ directory)
- OCaml with js_of_ocaml package

## Building

### Simple Build

To build the project with the default settings, run:

```
./build.sh
```

This will compile the project and generate `docs/js/web.js`.

### Manual Build

You can also use make directly:

```
make
```

### Cleaning

To clean the build artifacts:

```
make clean
```

## Deployment

The compiled project is deployed to:

- `docs/js/web.js`: The compiled JavaScript
- `docs/index.html`: The main HTML file

## Development

The main source file is `src/web.ml`. Other source files include:

- `src/code_execution.ml`: Code execution functionality
- `src/dom_utils.ml`: DOM utilities
- `src/editor_utils.ml`: Editor utilities
- And others
