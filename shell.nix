{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let ocamlPackages = ocaml-ng.ocamlPackages_4_14;
    pkg = ocamlPackages.callPackage ./. { };
in mkShell {
  inputsFrom = [ pkg ];
  buildInputs = pkg.checkInputs ++ [
    inotify-tools
    ocamlPackages.merlin
    ocamlformat
    ocamlPackages.ocp-indent
    ocamlPackages.utop
  ];
}