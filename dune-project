(lang dune 3.19)

(name cure2)

(generate_opam_files true)

(source
 (github ahrefs/cure2))

(authors "Emile Trotignon <emile.trotignon@gmail.com>")

(maintainers "Emile Trotignon <emile.trotignon@gmail.com>")

(license MIT)

(documentation https://ocaml.org/p/cure2)

(package
 (name cure2)
 (synopsis "Re-like combinators for Re2")
 (description "No need to remember regexp syntax to use Re2.")
 (depends
  (ocaml
   (>= 4.08))
  (re2
   (>= v0.16.0))
  (alcotest
   (and
    :with-test
    (>= 1.8.0))))
 (tags
  ("regexp" "regex")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
