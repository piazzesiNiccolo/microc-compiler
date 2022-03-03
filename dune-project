(lang dune 3.0)

(using menhir 2.1)

(name microc-compiler)

(package
  (name microc-compiler)
  (depends
    (ocaml 
      (>="4.10.0"))
    ppx_deriving
    menhir
    llvm
    llvm.analysis
    llvm.bitwriter
    llvm.scalar_opts))