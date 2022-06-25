{ lib, nix-gitignore, buildDunePackage, pcre, menhir, menhirLib, readline, ocaml_pcre }:

buildDunePackage rec {
  pname = "polaris";
  version = "0.1.0";
  duneVersion = "3";

  minimalOCamlVersion = "4.14";

  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  checkInputs = [ ];
  buildInputs = [ ocaml_pcre menhir menhirLib readline ];
  passthru.checkInputs = checkInputs;

  meta = { homepage = "https://github.com/Innf107/polaris"; };
}
