(lang dune 3.0)

(name polaris)

(generate_opam_files true)

(source
 (github Innf107/polaris))

(authors "Prophet")

(maintainers "Prophet")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name polaris)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  menhir
  menhirLib
  re
  yojson
  ppx_deriving
  ppx_protocol_conv
  bestline
  eio
  eio_main)
 (tags
  (topics "to describe" your project)))

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
