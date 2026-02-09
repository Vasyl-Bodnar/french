(lang dune 3.21)

(name french)

(generate_opam_files true)

(source
 (github Vasyl-Bodnar/french))

(authors "Vasyl Bodnar <vasyl_bodnar@outlook.com>")

(license LICENSE)

;; (documentation https://url/to/documentation)

(package
 (name french)
 (synopsis "SAT solver in OxCaml")
 (description "Simulated Annealing SAT solver in OxCaml")
 (depends ocaml)
 (tags (sat)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
