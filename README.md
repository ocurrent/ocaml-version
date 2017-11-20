## ocaml-version - Manipulate, parse and generate OCaml version strings

This library provides facilities to parse version numbers of the OCaml
compiler, and enumerates the various official OCaml releases and configuration
variants.

OCaml version numbers are of the form `major.minor.patch+extra`, where the
`patch` and `extra` fields are optional.  This library offers the following
functionality:

- Functions to parse and serialise OCaml version numbers.
- Enumeration of official OCaml version releases.
- Test compiler versions for a particular feature (e.g. the `bytes` type)
- [opam](https://opam.ocaml.org) compiler switch enumeration.

Browse the [API documentation](http://docs.mirage.io/ocaml-version) for more
details.
