# Polaris

## What is Polaris?
A scripting language with shell scripting capabilities (WIP) 

## How to install Polaris
```bash
sudo pacman -S opam
opam init
eval $(opam env)
opam switch create 4.14.0
eval $(opam env -switch=4.14.0)
opam install menhir
opam install dune
dune build @install
dune install polaris
sudo cp $HOME/.opam/4.14.0/bin/polaris /usr/bin/
