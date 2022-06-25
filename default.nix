{ lib
, nix-gitignore
, buildDunePackage
, pcre
, menhir
, menhirLib
, readline
, ocaml_pcre
, bestline ? (let repo = fetchGit {
        url = "ssh://git@github.com/Innf107/ocaml-bestline.git"; 
        ref= "nix";
        rev = "36f7495ca859a3ef3d391e9bf731e1419dc5da2e";
    }; in
    import "${repo.outPath}" { lib = lib; nix-gitignore = nix-gitignore; buildDunePackage = buildDunePackage; })
}:

buildDunePackage rec {
  pname = "polaris";
  version = "0.1.0";
  duneVersion = "3";

  minimalOCamlVersion = "4.14";

  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  checkInputs = [ ];
  buildInputs = [ ocaml_pcre menhir menhirLib readline bestline ];
  passthru.checkInputs = checkInputs;

  meta = { homepage = "https://github.com/Innf107/polaris"; };
}
