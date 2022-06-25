#!/usr/bin/bash

set -euo pipefail


if [ -z $(opam switch list -s | grep '^4.14.0$') ]; then
    echo "Creating opam switch for 4.14.0"
    opam switch create 4.14.0
fi

eval $(opam env --switch=4.14.0 --set-switch)

echo "Installing dune"
opam install dune

echo "Installing menhir"
opam install menhir

echo "Installing bestline"
opam pin add bestline ssh://git@github.com/Innf107/ocaml-bestline

echo "Installing other dependencies"
opam install . --deps-only

echo "Building project with dune"
dune build
dune install


echo "Handing control to the polairs build script"
dune exec polaris -- install.pls $@
