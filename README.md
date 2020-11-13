# Distributed, Reliable, Easy Shell

Performing tasks that are distributed among many systems currently fail to achieve each of the goals of being reliable, easy to set up, closely resembling an existing technology, and friendly to users without distributed systems knowledge.
Because distributed systems are inherently unreliable, succeeding in these goals require the system to provide an abstraction for error handling and recovery that account for the many failure cases that may arise.
This thesis project attempts to define a set of error handling semantics that is simple to use, but powerful enough to be applied to a diverse set of use cases.
The project will also involve implementing a shell-based programming language to demonstrate the usefulness of the error handling semantics as defined.
Using this language, anyone with basic shell knowledge should be able to write and run scripts that runs over multiple machines reliably.
This will greatly aid in increasing the accessibility and simplifying the process of running distributed programs in domains including systems administration and scientific computing.

## Setup

Follow the [setup guide](https://dev.realworldocaml.org/install.html) from [Real World OCaml](https://dev.realworldocaml.org/) to set up the following:
- The Ocaml package manager `opam`
- OCaml version `4.10.0`
- Make sure at least the following `opam` packages are installed with `opam install <package_name>`
  - `dune base core async core_bench merlin angstrom utop ocamlformat ocaml-lsp-server`
- If using VSCode, install the extension `OCaml Platform` from `OCaml Labs`
  - For other editors, check on the Real World OCaml setup guide linked above

## Building

Running `make` on the top level directory builds an executable (using `dune build`) and symbolic links the executable to `dresh.exe`.

Running `dune runtest` runs the tests in the `/test` directory, with any failing tests being indicated in the output.

Running `dune promote` accepts the program as being correct, and adjusts the output of the test cases to the program's output.