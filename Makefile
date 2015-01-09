.PHONY: all clean

languages = Haskell OCaml Rust Rust_parallel \
	    C++98 C++98_omp C++14 C++14_boost C D Go \
	    NodeJS NodeJS_async CoffeeScript CoffeeScript_parallel \
	    Java Chicken Racket FreePascal Erlang CommonLisp_opt

all: $(languages)

.PHONY: $(languages)
$(languages): %:
	$(MAKE) -C $*

clean:
	for d in $(languages); do $(MAKE) clean -C "$$d"; done
