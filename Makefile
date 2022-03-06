.PHONY: all clean

languages = C C++11 C++14 C++14_boost C++17 C++98 \
		Chicken CoffeeScript CoffeeScript_parallel \
		CommonLisp_opt Crystal CSharp Cython \
		D D_parallel Dart Erlang FreePascal \
		Haskell Haskell_parallel \
		Go Go_goroutine \
		Java Nim NodeJS NodeJS_async NodeJS_cluster \
		OCaml Racket Racket_parallel Rust Rust_parallel \
		Scala Vala ChezScheme
		# OCaml_lwt

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
