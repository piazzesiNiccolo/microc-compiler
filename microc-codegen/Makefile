TARGET=microcc


default: get_errors native


get_errors:
	 menhir --compile-errors  src/errors.messages  src/parser.mly > src/errors.ml
$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind $@
	mv $@ $*

clean:
	ocamlbuild -clean

.PHONY: clean default


scan_test: scan_test.native