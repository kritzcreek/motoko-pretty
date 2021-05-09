# motoko-pretty

A pretty printer library for Motoko.

Ported from [purescript-dodo-printer](https://github.com/natefaubion/purescript-dodo-printer).

All credit goes to Nate Faubion

## How to develop

- Write your library code in `*.mo` source files in the `src/` directory.
- Run `make check` to make sure your changes compile (or use the
  VSCode extension to get quicker feedback)
- Add tests to the source files in the `test/` directory, and run them
  with `make test`. The project template is set up to include
  motoko-matchers.
- Generate API documentation locally by running `make docs` and then
  open the resulting `docs/index.html` in your browser

## API Documentation

API documentation for this library can be found at https://kritzcreek.github.io/motoko-pretty

## License

motoko-pretty is distributed under the terms of the Apache License (Version 2.0).

See LICENSE for details.
