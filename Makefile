.PHONY: all clean

languages=Haskell OCaml Rust C++ C Go NodeJS Java

all:
	for d in $(languages); do $(MAKE) -C "$$d" || exit $?; done

clean:
	for d in $(languages); do $(MAKE) clean -C "$$d"; done
