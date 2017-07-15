.PHONY: all clean

%: %.ml
	opam init -n
	opam install -y lwt.3.0.0
	ocamlfind \
		ocamlopt -ccopt -O3 -noassert -unsafe \
		-linkpkg \
		-package lwt.unix \
		-package lwt.ppx -ppxopt lwt.ppx,-no-debug \
		-o $@ $<
	strip $@

all: swapview

clean:
	-rm -f *.cm* swapview{,.o}
