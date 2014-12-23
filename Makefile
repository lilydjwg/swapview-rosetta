.PHONY: all clean

languages=Haskell OCaml

all:
	for d in $(languages); do $(MAKE) -C "$$d"; done

clean:
	for d in $(languages); do $(MAKE) clean -C "$$d"; done
