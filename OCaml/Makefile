.PHONY: all clean

%: %.ml
	ocamlopt str.cmxa $< -o $@
	strip $@


all: swapview

clean:
	-rm -f *.cm* swapview
