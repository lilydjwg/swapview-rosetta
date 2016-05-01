.PHONY: all clean

languages = Haskell Haskell2 OCaml Rust Rust_parallel \
	    C++98 C++11 C++14 C++14_boost C CSharp D D_parallel Go Vala \
	    Nim NodeJS NodeJS_async CoffeeScript CoffeeScript_parallel \
	    Java Chicken Racket FreePascal Erlang CommonLisp_opt \
	    Dart ChezScheme

all: $(languages)

.PHONY: $(languages)
$(languages): %:
	$(MAKE) -C $*

clean:
	for d in $(languages); do $(MAKE) clean -C "$$d"; done

maketime: clean
	> $@
	for d in $(languages); do time -f "%e $$d" -a -o $@ $(MAKE) -C "$$d"; done
	sort $@ -o $@  -t" " -k1 -n
	cat $@
