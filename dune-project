(lang dune 3.9)

(name cligraph)

(generate_opam_files true)

(source
 (github TrN000/cligraph))

(authors "Nicolas Trutmann")

(maintainers "Nicolas Trutmann")

(license LICENSE)

(package
 (name cligraph)
 (synopsis "create 2d plots in the terminal")
 (description "This project provides a library to create plots as strings that can be printed in the terminal. requires UTF-8 and ansi escape codes to work.")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
