(lang dune 3.0)

(using menhir 2.1)

(cram enable)

(authors
  "Benjamin Pierce" "Leo White" "Jeremy Yallop")

(maintainers "Jeremy Yallop <yallop@gmail.com>")

(source
  (github yallop/fomega))

(package
  (name fomega)
  (synopsis "System Fω interpreter")
  (tags (fomega interpreter system f systemf))
  (depends
    ocaml
    menhir))
