utext — Unicode text for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

utext is an OCaml type for storing and processing Unicode text.

utext is distributed under the ISC license and depends on [pvec][pvec]

[pvec]: http://erratique.ch/software/pvec 

Homepage: http://erratique.ch/software/utext  

## Installation

utext can be installed with `opam`:

    opam install utext

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
utext`.

[doc]: http://erratique.ch/software/utext/doc

## Sample programs

If you installed utext with `opam` sample programs are located in
the directory `opam var utext:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
