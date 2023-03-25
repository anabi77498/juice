.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

start-juice:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f juice.zip
	zip -r juice.zip . -x@exclude.lst

clean:
	dune clean
	rm -f juice.zip