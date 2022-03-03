(lang dune 3.0)



(name microc-compiler)

(license GPL3)

(authors 
"Niccolò Piazzesi")

(maintainers
  "Niccolò Piazzesi")

(generate_opam_files true)

(using menhir 2.1)

(package
  (name microc-compiler)
  (synopsis "microcc is a simple llvm based compiler for a small subset of called microc")
  (description "microcc is a simple llvm based compiler for a small subset of called microc. 
  It was developed as the final project of the course Languages, Compilers, and Intepreters at UniPi. ")
  (depends
    (ocaml 
      (>="4.10.0"))
    dune 
    (odoc :with-doc)
    ppx_deriving
    menhir
    (llvm
      (>= "10.0.0"))))