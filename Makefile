.PHONY: all bench clean
SHELL := /bin/zsh
TIMES := 3

languages=Haskell OCaml Rust C++98 C++14 C Go NodeJS NodeJS_async Java Chicken \
	  CoffeeScript CoffeeScript_parallel Racket FreePascal

all:
	for d in $(languages); do $(MAKE) -C "$$d" || exit $?; done

clean:
	for d in $(languages); do $(MAKE) clean -C "$$d"; done

bench:
	@echo 'Time\tLanguage'
	@for d in $(languages); do { time (repeat $(TIMES) $$d/swapview*(X) >/dev/null);} |& awk "{print \$$(NF-1)/$(TIMES)\"\\t$$d\"}"; done | sort -n
