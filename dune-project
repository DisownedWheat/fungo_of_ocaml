(lang dune 3.16)

(name fungo)

(generate_opam_files true)

(source
 (github DisownedWheat/fungo_of_ocaml))

(authors "Rob Larnach")

(maintainers "Rob Larnach")

(license MIT)

(documentation https://github.com/DisownedWheat/fungo_of_ocaml)

(package
 (name fungo)
 (synopsis "An Ocaml inspired language that compiles to Go")
 (description "A longer description will be forthcoming")
 (depends ocaml dune)
 (tags
  (compiler)))

