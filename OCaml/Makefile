.PHONY: all clean

%: %.ml
	ocamlopt -ccopt -O3 -noassert -unsafe $< -o $@
	strip $@


all: swapview

clean:
	-rm -f *.cm* swapview
