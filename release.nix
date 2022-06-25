{ pkgs ? import <nixpkgs> { } }:

pkgs.ocaml-ng.ocamlPackages_4_14.callPackage ./. { }