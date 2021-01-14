# Accessible, Reliable, Distributed Shell (Shard)

Performing tasks that are distributed among many systems currently fail to achieve each of the goals of being reliable, easy to set up, closely resembling an existing technology, and friendly to users without distributed systems knowledge.
Because distributed systems are inherently unreliable, succeeding in these goals require the system to provide an abstraction for error handling and recovery that account for the many failure cases that may arise.
This thesis project attempts to define a set of error handling semantics that is simple to use, but powerful enough to be applied to a diverse set of use cases.
The project will also involve implementing a shell-based programming language to demonstrate the usefulness of the error handling semantics as defined.
Using this language, anyone with basic shell knowledge should be able to write and run scripts that runs over multiple machines reliably.
This will greatly aid in increasing the accessibility and simplifying the process of running distributed programs in domains including systems administration and scientific computing.

## Setup

Follow the [setup guide](https://dev.realworldocaml.org/install.html) from [Real World OCaml](https://dev.realworldocaml.org/) to set up the following:
- The Ocaml package manager `opam`
  - `sudo apt install opam`
  - `opam init`
- OCaml version `4.10.0`
  - `opam switch create 4.10.0`
  - `eval $(opam env)`
  - Make sure `eval $(opam env)` is in your `.bashrc` file so it gets automatically ran
- Make sure at least the following `opam` packages are installed with the following installation command
  - `opam install dune base core async core_bench merlin angstrom utop ocamlformat ocaml-lsp-server`
  - The package `pkg-config` may need to be installed as a dependency
    - `sudo apt install pkg-config`
- If using VSCode, install the extension `OCaml Platform` from `OCaml Labs`
  - For other editors, check on the Real World OCaml setup guide linked above

- TODO: add information about `libssh`

## Building

Running `make` on the top level directory builds an executable (using `dune build`) and symbolic links the executable to `shard.exe`.

Running `dune runtest` runs the tests in the `/test` directory, with any failing tests being indicated in the output.

Running `dune promote` accepts the program as being correct, and adjusts the output of the test cases to the program's output.


## License

The source files, build files, and documentation in this repository are dual licensed under the MIT license and the LGPL 2.1+ license (version 2.1 or any future version), with the following exceptions:
- The files in the directory [src/mllibssh_extended] is modified from the project [Mllibssh](https://gitlab.com/pinotree/mllibssh) licensed under the [LGPL 2.1+ license](https://gitlab.com/pinotree/mllibssh/-/blob/master/COPYING).
- The files in the directory [src/angstrom_extended] is modified from the project [Angstrom](https://github.com/inhabitedtype/angstrom/) licensed under the [3-clause BSD license](https://github.com/inhabitedtype/angstrom/blob/master/LICENSE).

The thesis writing, including TeX and PDF files, are not licensed under the above license.

Note that the built executable statically links [libssh](https://www.libssh.org/), which is licensed under the [LGPL 2.1 license](https://git.libssh.org/projects/libssh.git/tree/COPYING).

