(lang dune 2.7)
(generate_opam_files true)

(name datalog)
(version 0.7)
(source (github c-cube/datalog))
(maintainers "simon.cruanes.2007@m4x.org")
(authors "Simon Cruanes")
(license "BSD-2-Clause")

(package
 (name datalog)
 (synopsis "An in-memory datalog implementation for OCaml")
 (tags (datalog relational query prolog))
 (depends
  (dune ( >= "2.7" ))
  (ocaml ( >= "4.08" ))
  (odoc :with-doc)
  (mdx (and ( >= "1.3" ) :with-test))))
