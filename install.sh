#!/usr/bin/bash

set -euo pipefail

echo "Building project with dune"
dune build
dune install

echo "Handing control to the polairs build script"
dune exec polaris -- install.pol
