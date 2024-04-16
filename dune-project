(lang dune 2.0)
(using menhir 2.0)
(generate_opam_files true)

(name smtlib-utils)
(version 0.5)

(maintainers "simon.cruanes.2007@m4x.org")
(authors "Simon Cruanes")
(license "BSD-2-Clause")
(homepage "https://github.com/c-cube/smtlib-utils/")
(source (github c-cube/smtlib-utils))

(package
  (name smtlib-utils)
(synopsis "Parser for SMTLIB2")
(tags ("SMTLIB" "smt2" "parse" "logic"))
(depends
  ("dune" ( >= "2.0" ))
  ("menhir" (and :build (>= "20180523")))
  ("odoc" :with-doc)
  ("ocaml" ( >= "4.08.0" ))
  ("trace" :with-test )
  ("trace-tef" :with-test ))

)
