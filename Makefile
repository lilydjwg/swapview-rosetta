.PHONY: all clean

languages=Haskell OCaml Rust C++98 C++14 C Go NodeJS NodeJS_async Java Chicken \
	  CoffeeScript CoffeeScript_parallel Racket FreePascal CommonLisp_opt

all:
	for d in $(languages); do $(MAKE) -C "$$d" || exit $?; done

clean:
	for d in $(languages); do $(MAKE) clean -C "$$d"; done
