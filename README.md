swapview is a simple program to view processes' swap usage on Linux. This is intended to be a lilydjwg's version of Hello World program.

How to play
----

Install all the dependencies for your interested versions, then run `make -k`. It will build all that can be built.

To test and see the speed comparison, either use `bench.py` or `run_benchmark`. `bench.py` uses Python 3.4+ and depends on `python-toml`.

`run_benchmark` is a Rust version. Install latest Rust and Cargo, then change your working directory to `benchmark` and run `cargo build --release`. If the build fails, it's because your Rust is too old or too new....

Run `./run_benchmark <benchmark.toml` and wait for it to finish. Failed ones (e.g. because you don't have the dependencies installed) will be marked as failed so you don't need to edit `benchmark.toml` to disable the ones you can't run.

You can give `./run_benchmark` names to selectively run some versions, e.g.

    ./run_benchmark C C++14 'Rust*' <benchmark.toml

Implementions and their requirements
----

Of course you should have corresponding compilers / interpreters installed.
You can (edit and then) run `make` to build all that needs to be built.

* Bash
* Bash_parallel, Bash version using GNU parallel
* C
* C++98
* C++98_omp, openmp paralleled version
* C++14
* C++14_boost, C++ version using the boost library
* CSharp (mono)
* ChezScheme
* Chicken, format (will be installed by `make`)
* CommonLisp_opt, sbcl
* CommonLisp_old, sbcl, maynbe others also work
* CoffeeScript, requires promise (will be installed by `make`)
* CoffeeScript_parallel, a parallel version, requires promise (will be installed by `make`)
* D, `dmd` or `ldmd` (LLVM version)
* Dart
* Elixir
* Erlang
* Go
* Guile, tested with 2.0.11
* Haskell, requires `strict` (install from `aur/haskell-strict` or `haskell-core/haskell-strict` or by `cabal install strict`)
* Haskell2, another better Haskell version using more dependencies
* Java, >= Java 8
* Lua, requires lua-filesystem, works with 5.1, 5.2 and LuaJIT
* NodeJS, requires sprintf (will be installed by `make`)
* NodeJS_async, another NodeJS version which use async I/O, requires sprintf and async (will be installed by `make`)
* OCaml
* FreePascal
* Perl
* Python, works with Python 2 & 3
* Python3_bytes
* R
* Racket
* Ruby and Rubinius
* Rust, use git version please
* Scala 2.11.6
* Vala, requires `glib2` (`libglib-2.0` and `libgio-2.0`)
* Tcl, >= 8.6

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

My Result
----

Updated at 2015-09-26:

<pre>
<span style="color:lime;font-weight:bold;">               C++98_omp</span>: top: <span style="color:white;font-weight:bold;">  24.49</span>, min:   23.22, avg:   27.08, max:   34.18, mdev:    3.36, cnt:  20
<span style="color:lime;font-weight:bold;">             C++14_boost</span>: top: <span style="color:white;font-weight:bold;">  40.26</span>, min:   39.91, avg:   43.27, max:   87.06, mdev:   10.11, cnt:  20
<span style="color:lime;font-weight:bold;">                   C++98</span>: top: <span style="color:white;font-weight:bold;">  40.35</span>, min:   39.54, avg:   41.02, max:   44.15, mdev:    1.07, cnt:  20
<span style="color:lime;font-weight:bold;">                       C</span>: top: <span style="color:white;font-weight:bold;">  43.02</span>, min:   42.67, avg:   43.64, max:   46.43, mdev:    0.91, cnt:  20
<span style="color:lime;font-weight:bold;">                   C++14</span>: top: <span style="color:white;font-weight:bold;">  47.76</span>, min:   47.31, avg:   49.40, max:   55.81, mdev:    2.43, cnt:  20
<span style="color:lime;font-weight:bold;">                     Nim</span>: top: <span style="color:white;font-weight:bold;">  50.00</span>, min:   49.40, avg:   54.63, max:  133.19, mdev:   18.04, cnt:  20
<span style="color:lime;font-weight:bold;">                    Rust</span>: top: <span style="color:white;font-weight:bold;">  52.46</span>, min:   49.43, avg:   61.78, max:   76.24, mdev:    9.98, cnt:  20
<span style="color:lime;font-weight:bold;">           Rust_parallel</span>: top: <span style="color:white;font-weight:bold;">  53.24</span>, min:   30.86, avg:   66.70, max:   99.91, mdev:   17.71, cnt:  20
<span style="color:lime;font-weight:bold;">            Go_goroutine</span>: top: <span style="color:white;font-weight:bold;">  53.66</span>, min:   52.44, avg:   56.80, max:   78.63, mdev:    5.73, cnt:  20
<span style="color:lime;font-weight:bold;">              D_parallel</span>: top: <span style="color:white;font-weight:bold;">  63.37</span>, min:   60.26, avg:   66.44, max:   84.44, mdev:    4.87, cnt:  20
<span style="color:lime;font-weight:bold;">                     PHP</span>: top: <span style="color:white;font-weight:bold;">  64.23</span>, min:   62.49, avg:   77.16, max:  298.22, mdev:   50.74, cnt:  20
<span style="color:lime;font-weight:bold;">                  LuaJIT</span>: top: <span style="color:white;font-weight:bold;">  64.50</span>, min:   64.05, avg:   65.56, max:   73.98, mdev:    2.10, cnt:  20
<span style="color:lime;font-weight:bold;">         D_parallel_llvm</span>: top: <span style="color:white;font-weight:bold;">  64.55</span>, min:   61.96, avg:   70.06, max:  119.61, mdev:   11.99, cnt:  20
<span style="color:lime;font-weight:bold;">                Haskell2</span>: top: <span style="color:white;font-weight:bold;">  72.38</span>, min:   71.25, avg:   84.11, max:  266.24, mdev:   41.88, cnt:  20
<span style="color:lime;font-weight:bold;">                  D_llvm</span>: top: <span style="color:white;font-weight:bold;">  72.83</span>, min:   72.07, avg:   83.14, max:  248.75, mdev:   38.06, cnt:  20
<span style="color:lime;font-weight:bold;">                      Go</span>: top: <span style="color:white;font-weight:bold;">  73.44</span>, min:   70.88, avg:   76.60, max:  101.72, mdev:    6.22, cnt:  20
<span style="color:lime;font-weight:bold;">                       D</span>: top: <span style="color:white;font-weight:bold;">  76.97</span>, min:   76.18, avg:   79.68, max:   95.74, mdev:    4.89, cnt:  20
<span style="color:lime;font-weight:bold;">                   Lua51</span>: top: <span style="color:white;font-weight:bold;">  77.03</span>, min:   76.68, avg:   78.10, max:   86.71, mdev:    2.24, cnt:  20
<span style="color:lime;font-weight:bold;">                   Lua52</span>: top: <span style="color:white;font-weight:bold;">  80.19</span>, min:   79.59, avg:   89.59, max:  131.33, mdev:   17.10, cnt:  20
<span style="color:lime;font-weight:bold;">              FreePascal</span>: top: <span style="color:white;font-weight:bold;">  82.87</span>, min:   82.01, avg:   95.05, max:  290.07, mdev:   44.81, cnt:  20
<span style="color:lime;font-weight:bold;">                 Python2</span>: top: <span style="color:white;font-weight:bold;">  83.41</span>, min:   82.83, avg:   84.78, max:   91.75, mdev:    2.17, cnt:  20
<span style="color:lime;font-weight:bold;">                   Lua53</span>: top: <span style="color:white;font-weight:bold;">  83.62</span>, min:   82.83, avg:   86.54, max:  124.93, mdev:    8.91, cnt:  20
<span style="color:lime;font-weight:bold;">                    Vala</span>: top: <span style="color:white;font-weight:bold;">  88.51</span>, min:   87.82, avg:   91.17, max:  101.02, mdev:    4.16, cnt:  20
<span style="color:lime;font-weight:bold;">           Python3_bytes</span>: top: <span style="color:white;font-weight:bold;">  98.98</span>, min:   98.43, avg:  100.11, max:  103.67, mdev:    1.41, cnt:  20
<span style="color:lime;font-weight:bold;">                    Perl</span>: top: <span style="color:white;font-weight:bold;"> 104.16</span>, min:  102.65, avg:  106.57, max:  122.93, mdev:    4.53, cnt:  20
<span style="color:lime;font-weight:bold;">                 Chicken</span>: top: <span style="color:white;font-weight:bold;"> 108.90</span>, min:  102.72, avg:  111.81, max:  125.27, mdev:    4.89, cnt:  20
<span style="color:lime;font-weight:bold;">                 Python3</span>: top: <span style="color:white;font-weight:bold;"> 115.59</span>, min:  114.19, avg:  116.80, max:  121.04, mdev:    1.72, cnt:  20
<span style="color:lime;font-weight:bold;">             PyPy3_bytes</span>: top: <span style="color:white;font-weight:bold;"> 124.83</span>, min:  123.15, avg:  129.14, max:  144.56, mdev:    5.41, cnt:  20
<span style="color:lime;font-weight:bold;">                  NodeJS</span>: top: <span style="color:white;font-weight:bold;"> 137.89</span>, min:  136.34, avg:  140.09, max:  152.85, mdev:    3.71, cnt:  20
<span style="color:lime;font-weight:bold;">                    Ruby</span>: top: <span style="color:white;font-weight:bold;"> 146.09</span>, min:  144.28, avg:  148.69, max:  173.50, mdev:    5.92, cnt:  20
<span style="color:lime;font-weight:bold;">                  Erlang</span>: top: <span style="color:white;font-weight:bold;"> 166.18</span>, min:  156.59, avg:  175.74, max:  205.63, mdev:   11.85, cnt:  20
<span style="color:lime;font-weight:bold;">                     Tcl</span>: top: <span style="color:white;font-weight:bold;"> 171.40</span>, min:  170.01, avg:  176.73, max:  230.59, mdev:   12.81, cnt:  20
<span style="color:lime;font-weight:bold;">                    PyPy</span>: top: <span style="color:white;font-weight:bold;"> 178.68</span>, min:  174.57, avg:  202.84, max:  601.18, mdev:   91.48, cnt:  20
<span style="color:lime;font-weight:bold;">                   PyPy3</span>: top: <span style="color:white;font-weight:bold;"> 185.52</span>, min:  180.64, avg:  312.99, max: 2656.70, mdev:  537.72, cnt:  20
<span style="color:lime;font-weight:bold;">            NodeJS_async</span>: top: <span style="color:white;font-weight:bold;"> 186.94</span>, min:  181.33, avg:  199.95, max:  225.19, mdev:   14.67, cnt:  20
<span style="color:lime;font-weight:bold;">   CoffeeScript_parallel</span>: top: <span style="color:white;font-weight:bold;"> 191.78</span>, min:  178.10, avg:  209.31, max:  242.15, mdev:   20.91, cnt:  20
<span style="color:lime;font-weight:bold;">            CoffeeScript</span>: top: <span style="color:white;font-weight:bold;"> 192.69</span>, min:  172.32, avg:  213.77, max:  248.27, mdev:   22.95, cnt:  20
<span style="color:lime;font-weight:bold;">                  CSharp</span>: top: <span style="color:white;font-weight:bold;"> 197.32</span>, min:  191.22, avg:  215.08, max:  252.60, mdev:   21.86, cnt:  20
<span style="color:lime;font-weight:bold;">                   OCaml</span>: top: <span style="color:white;font-weight:bold;"> 302.12</span>, min:  298.66, avg:  316.89, max:  378.45, mdev:   21.79, cnt:  20
<span style="color:lime;font-weight:bold;">                    Java</span>: top: <span style="color:white;font-weight:bold;"> 372.92</span>, min:  243.77, avg:  411.14, max:  472.47, mdev:   59.60, cnt:  20
<span style="color:lime;font-weight:bold;">                 Haskell</span>: top: <span style="color:white;font-weight:bold;"> 380.17</span>, min:  378.35, avg:  392.75, max:  580.18, mdev:   43.16, cnt:  20
<span style="color:lime;font-weight:bold;">         Racket_compiled</span>: top: <span style="color:white;font-weight:bold;"> 396.20</span>, min:  375.71, avg:  437.81, max:  513.44, mdev:   48.19, cnt:  20
<span style="color:lime;font-weight:bold;">                  Racket</span>: top: <span style="color:white;font-weight:bold;"> 402.99</span>, min:  371.00, avg:  467.97, max:  823.70, mdev:   97.84, cnt:  20
<span style="color:lime;font-weight:bold;">          CommonLisp_opt</span>: top: <span style="color:white;font-weight:bold;"> 446.21</span>, min:  377.54, avg:  564.21, max: 1746.35, mdev:  279.13, cnt:  20
<span style="color:lime;font-weight:bold;">                   Guile</span>: top: <span style="color:white;font-weight:bold;"> 471.11</span>, min:  467.24, avg:  476.67, max:  506.51, mdev:    8.37, cnt:  20
<span style="color:lime;font-weight:bold;">                   Scala</span>: top: <span style="color:white;font-weight:bold;"> 503.63</span>, min:  470.86, avg:  575.36, max: 1545.65, mdev:  224.16, cnt:  20
<span style="color:lime;font-weight:bold;">                       R</span>: top: <span style="color:white;font-weight:bold;"> 518.63</span>, min:  515.56, avg:  552.70, max: 1106.06, mdev:  127.07, cnt:  20
<span style="color:lime;font-weight:bold;">          CommonLisp_old</span>: top: <span style="color:white;font-weight:bold;"> 561.53</span>, min:  470.90, avg:  611.44, max:  700.56, mdev:   68.65, cnt:  20
<span style="color:lime;font-weight:bold;">                   Julia</span>: top: <span style="color:white;font-weight:bold;"> 569.87</span>, min:  564.74, avg:  584.26, max:  710.40, mdev:   30.69, cnt:  20
<span style="color:lime;font-weight:bold;">                  Elixir</span>: top: <span style="color:white;font-weight:bold;"> 643.60</span>, min:  632.59, avg:  685.73, max: 1285.45, mdev:  138.15, cnt:  20
<span style="color:lime;font-weight:bold;">              POSIX_dash</span>: top: <span style="color:white;font-weight:bold;">1160.31</span>, min: 1144.75, avg: 1168.97, max: 1189.14, mdev: 4186.23, cnt:  20
<span style="color:lime;font-weight:bold;">                    Bash</span>: top: <span style="color:white;font-weight:bold;">1237.99</span>, min: 1218.74, avg: 1269.55, max: 1635.89, mdev: 4187.10, cnt:  20
<span style="color:lime;font-weight:bold;">           Bash_parallel</span>: top: <span style="color:white;font-weight:bold;">1302.56</span>, min: 1282.07, avg: 1320.14, max: 1488.69, mdev: 4186.41, cnt:  20
<span style="color:lime;font-weight:bold;">               POSIX_zsh</span>: top: <span style="color:white;font-weight:bold;">1329.94</span>, min: 1304.03, avg: 1345.76, max: 1383.39, mdev: 4186.26, cnt:  20
<span style="color:lime;font-weight:bold;">              POSIX_bash</span>: top: <span style="color:white;font-weight:bold;">1470.59</span>, min: 1437.27, avg: 1485.59, max: 1539.51, mdev: 4074.62, cnt:  20
</pre>
