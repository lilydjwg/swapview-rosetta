swapview is a simple program to view processes' swap usage on Linux. This is intended to be a lilydjwg's version of Hello World program.

Implementions and their requirements
----

Of course you should have corresponding compilers / interpreters installed.
You can (edit and then) run `make` to build all that needs to be built.

* Bash, broken, please contribute
* C
* C++98
* C++14
* Chicken, format (will be installed by `make`)
* CommonLisp, sbcl
* CoffeeScript, requires promise (will be installed by `make`)
* CoffeeScript_parallel, a parallel version, requires promise (will be installed by `make`)
* Elixir
* Erlang
* Go
* Guile, tested with 2.0.11
* Haskell, requires `strict` (install from `aur/haskell-strict` or `haskell-core/haskell-strict` or by `cabal install strict`)
* Haskell2, another Haskell version which requires some external dependencies
* Java, >= Java 8
* Lua, requires lua-filesystem, works with 5.1, 5.2 and LuaJIT
* NodeJS, requires sprintf (will be installed by `make`)
* NodeJS_async, another NodeJS version which use async I/O, requires sprintf and async (will be installed by `make`)
* OCaml
* FreePascal
* Python, works with Python 2 & 3
* Python3_bytes
* Racket
* Ruby
* Rust, use git version please

Contributions
----

Contributions are welcome! Improve current ones, or submit new one. But make
sure your implementations meet the following requirements:

1. Must be readable and maintainable
2. Output exact the same format as other versions (but sorting may be
   unstable)
3. Try to be efficient
4. Please include a `Makefile` if appropriate
5. Don't forget to tell the compiler to optimize
