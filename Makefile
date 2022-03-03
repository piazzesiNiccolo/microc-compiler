

default:  native


native:
	dune build 

clean:
	dune clean

.PHONY: clean default native


