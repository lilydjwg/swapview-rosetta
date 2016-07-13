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
* Haskell, requires `haskell-strict`
* Haskell2, another better Haskell version using more dependencies
* Java, >= Java 8
* Lua, requires lua-filesystem, works with 5.1, 5.2, 5.3 and LuaJIT
* NodeJS, requires sprintf (will be installed by `make`)
* NodeJS_async, another NodeJS version which use async I/O, requires sprintf and async (will be installed by `make`)
* NodeJS_parallel, uses multiple worker processes
* OCaml
* FreePascal
* Perl
* Python, works with Python 2 & 3
* Python3_bytes
* R
* Racket
* Ruby and Rubinius
* Rust
* Scala
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

This is tested on Arch Linux, with latest versions of compilers and interpreters available there at testing time.

Updated at 2016-06-30:

<pre>
<span style="color:green;font-weight:bold;">               C++98_omp</span>: top: <span style="color:gray;font-weight:bold;">  74.73</span>, min:   66.75, avg:   80.83, max:   98.24, mdev:    7.60, cnt:  20
<span style="color:green;font-weight:bold;">                       C</span>: top: <span style="color:gray;font-weight:bold;"> 127.29</span>, min:  126.11, avg:  129.39, max:  135.80, mdev:    2.52, cnt:  20
<span style="color:green;font-weight:bold;">                   C++98</span>: top: <span style="color:gray;font-weight:bold;"> 130.31</span>, min:  129.05, avg:  132.34, max:  139.47, mdev:    2.64, cnt:  20
<span style="color:green;font-weight:bold;">                     Nim</span>: top: <span style="color:gray;font-weight:bold;"> 130.38</span>, min:  128.58, avg:  137.71, max:  175.65, mdev:   13.85, cnt:  20
<span style="color:green;font-weight:bold;">                   C++14</span>: top: <span style="color:gray;font-weight:bold;"> 130.71</span>, min:  129.39, avg:  136.72, max:  160.87, mdev:    9.22, cnt:  20
<span style="color:green;font-weight:bold;">             C++14_boost</span>: top: <span style="color:gray;font-weight:bold;"> 131.28</span>, min:  130.52, avg:  133.39, max:  141.74, mdev:    2.85, cnt:  20
<span style="color:green;font-weight:bold;">           Rust_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 140.81</span>, min:  112.09, avg:  152.31, max:  181.36, mdev:   15.86, cnt:  20
<span style="color:green;font-weight:bold;">            Go_goroutine</span>: top: <span style="color:gray;font-weight:bold;"> 144.36</span>, min:  120.02, avg:  162.45, max:  236.54, mdev:   25.14, cnt:  20
<span style="color:green;font-weight:bold;">         D_parallel_llvm</span>: top: <span style="color:gray;font-weight:bold;"> 156.60</span>, min:  147.47, avg:  181.32, max:  241.03, mdev:   30.52, cnt:  20
<span style="color:green;font-weight:bold;">              D_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 157.24</span>, min:  150.02, avg:  172.33, max:  221.68, mdev:   19.69, cnt:  20
<span style="color:green;font-weight:bold;">                    Rust</span>: top: <span style="color:gray;font-weight:bold;"> 163.32</span>, min:  141.11, avg:  180.63, max:  254.20, mdev:   26.55, cnt:  20
<span style="color:green;font-weight:bold;">                   OCaml</span>: top: <span style="color:gray;font-weight:bold;"> 174.59</span>, min:  167.63, avg:  184.09, max:  209.87, mdev:   12.69, cnt:  20
<span style="color:green;font-weight:bold;">                     PHP</span>: top: <span style="color:gray;font-weight:bold;"> 185.03</span>, min:  176.63, avg:  193.54, max:  228.99, mdev:   12.23, cnt:  20
<span style="color:green;font-weight:bold;">                  LuaJIT</span>: top: <span style="color:gray;font-weight:bold;"> 185.23</span>, min:  183.01, avg:  189.70, max:  208.75, mdev:    6.57, cnt:  20
<span style="color:green;font-weight:bold;">                  D_llvm</span>: top: <span style="color:gray;font-weight:bold;"> 188.85</span>, min:  185.56, avg:  194.24, max:  226.12, mdev:    9.13, cnt:  20
<span style="color:green;font-weight:bold;">                      Go</span>: top: <span style="color:gray;font-weight:bold;"> 188.96</span>, min:  185.57, avg:  195.25, max:  218.36, mdev:    8.92, cnt:  20
<span style="color:green;font-weight:bold;">                Haskell2</span>: top: <span style="color:gray;font-weight:bold;"> 200.23</span>, min:  196.68, avg:  210.17, max:  303.74, mdev:   24.02, cnt:  20
<span style="color:green;font-weight:bold;">                       D</span>: top: <span style="color:gray;font-weight:bold;"> 206.18</span>, min:  202.35, avg:  212.35, max:  240.61, mdev:   10.08, cnt:  20
<span style="color:green;font-weight:bold;">                   Lua51</span>: top: <span style="color:gray;font-weight:bold;"> 219.65</span>, min:  217.46, avg:  227.44, max:  282.32, mdev:   15.12, cnt:  20
<span style="color:green;font-weight:bold;">                 Python2</span>: top: <span style="color:gray;font-weight:bold;"> 221.80</span>, min:  216.60, avg:  231.70, max:  292.08, mdev:   16.51, cnt:  20
<span style="color:green;font-weight:bold;">                   Lua52</span>: top: <span style="color:gray;font-weight:bold;"> 225.89</span>, min:  220.27, avg:  230.43, max:  252.03, mdev:    6.89, cnt:  20
<span style="color:green;font-weight:bold;">                  NodeJS</span>: top: <span style="color:gray;font-weight:bold;"> 231.00</span>, min:  227.71, avg:  234.71, max:  242.62, mdev:    4.22, cnt:  20
<span style="color:green;font-weight:bold;">                   Lua53</span>: top: <span style="color:gray;font-weight:bold;"> 235.73</span>, min:  232.38, avg:  242.27, max:  278.49, mdev:   10.49, cnt:  20
<span style="color:green;font-weight:bold;">                    Vala</span>: top: <span style="color:gray;font-weight:bold;"> 238.68</span>, min:  235.69, avg:  248.14, max:  296.29, mdev:   14.87, cnt:  20
<span style="color:green;font-weight:bold;">              FreePascal</span>: top: <span style="color:gray;font-weight:bold;"> 239.32</span>, min:  236.86, avg:  251.10, max:  288.25, mdev:   15.65, cnt:  20
<span style="color:green;font-weight:bold;">                    Perl</span>: top: <span style="color:gray;font-weight:bold;"> 261.46</span>, min:  257.73, avg:  278.10, max:  340.33, mdev:   25.94, cnt:  20
<span style="color:green;font-weight:bold;">           Python3_bytes</span>: top: <span style="color:gray;font-weight:bold;"> 261.76</span>, min:  259.26, avg:  269.99, max:  299.11, mdev:   11.54, cnt:  20
<span style="color:green;font-weight:bold;">             PyPy3_bytes</span>: top: <span style="color:gray;font-weight:bold;"> 277.98</span>, min:  273.41, avg:  297.01, max:  410.77, mdev:   38.60, cnt:  20
<span style="color:green;font-weight:bold;">                    PyPy</span>: top: <span style="color:gray;font-weight:bold;"> 290.85</span>, min:  284.78, avg:  317.91, max:  434.03, mdev:   39.05, cnt:  20
<span style="color:green;font-weight:bold;">                 Python3</span>: top: <span style="color:gray;font-weight:bold;"> 305.46</span>, min:  301.06, avg:  315.28, max:  357.00, mdev:   14.32, cnt:  20
<span style="color:green;font-weight:bold;">                    Ruby</span>: top: <span style="color:gray;font-weight:bold;"> 309.99</span>, min:  304.72, avg:  327.48, max:  372.77, mdev:   21.87, cnt:  20
<span style="color:green;font-weight:bold;">         NodeJS_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 326.85</span>, min:  316.24, avg:  347.25, max:  451.25, mdev:   34.91, cnt:  20
<span style="color:green;font-weight:bold;">                 Chicken</span>: top: <span style="color:gray;font-weight:bold;"> 329.45</span>, min:  322.71, avg:  353.39, max:  513.69, mdev:   42.01, cnt:  20
<span style="color:green;font-weight:bold;">                  Erlang</span>: top: <span style="color:gray;font-weight:bold;"> 366.50</span>, min:  339.11, avg:  390.16, max:  476.80, mdev:   31.00, cnt:  20
<span style="color:green;font-weight:bold;">              ChezScheme</span>: top: <span style="color:gray;font-weight:bold;"> 406.35</span>, min:  399.14, avg:  429.98, max:  528.54, mdev:   33.57, cnt:  20
<span style="color:green;font-weight:bold;">                   PyPy3</span>: top: <span style="color:gray;font-weight:bold;"> 422.61</span>, min:  418.73, avg:  427.92, max:  450.91, mdev:    7.45, cnt:  20
<span style="color:green;font-weight:bold;">                     Tcl</span>: top: <span style="color:gray;font-weight:bold;"> 466.96</span>, min:  459.65, avg:  475.79, max:  522.68, mdev:   14.41, cnt:  20
<span style="color:green;font-weight:bold;">                  CSharp</span>: top: <span style="color:gray;font-weight:bold;"> 468.89</span>, min:  396.01, avg:  500.82, max:  631.11, mdev:   47.88, cnt:  20
<span style="color:green;font-weight:bold;">   CoffeeScript_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 564.80</span>, min:  351.00, avg:  613.40, max:  752.75, mdev:   77.55, cnt:  20
<span style="color:green;font-weight:bold;">            CoffeeScript</span>: top: <span style="color:gray;font-weight:bold;"> 567.97</span>, min:  471.83, avg:  601.48, max:  661.47, mdev:   43.20, cnt:  20
<span style="color:green;font-weight:bold;">            NodeJS_async</span>: top: <span style="color:gray;font-weight:bold;"> 660.61</span>, min:  633.43, avg:  687.55, max:  796.57, mdev:   36.33, cnt:  20
<span style="color:green;font-weight:bold;">                    Java</span>: top: <span style="color:gray;font-weight:bold;"> 853.59</span>, min:  824.68, avg:  887.36, max: 1016.02, mdev:   45.10, cnt:  20
<span style="color:green;font-weight:bold;">                    Dart</span>: top: <span style="color:gray;font-weight:bold;"> 918.39</span>, min:  705.92, avg:  987.41, max: 1084.45, mdev: 4187.41, cnt:  20
<span style="color:green;font-weight:bold;">                  Racket</span>: top: <span style="color:gray;font-weight:bold;">1022.06</span>, min:  755.98, avg: 1068.39, max: 1166.63, mdev: 4187.11, cnt:  20
<span style="color:green;font-weight:bold;">          CommonLisp_old</span>: top: <span style="color:gray;font-weight:bold;">1070.15</span>, min:  896.43, avg: 1113.44, max: 1243.69, mdev: 4186.87, cnt:  20
<span style="color:green;font-weight:bold;">          CommonLisp_opt</span>: top: <span style="color:gray;font-weight:bold;">1086.13</span>, min: 1067.23, avg: 1107.29, max: 1199.43, mdev: 4186.33, cnt:  20
<span style="color:green;font-weight:bold;">         Racket_compiled</span>: top: <span style="color:gray;font-weight:bold;">1088.63</span>, min: 1059.70, avg: 1118.23, max: 1239.85, mdev: 4186.42, cnt:  20
<span style="color:green;font-weight:bold;">                   Scala</span>: top: <span style="color:gray;font-weight:bold;">1139.17</span>, min:  929.79, avg: 1192.92, max: 1406.76, mdev: 4187.09, cnt:  20
<span style="color:green;font-weight:bold;">                   Guile</span>: top: <span style="color:gray;font-weight:bold;">1188.32</span>, min: 1167.39, avg: 1225.72, max: 1393.59, mdev: 4186.66, cnt:  20
<span style="color:green;font-weight:bold;">                 Haskell</span>: top: <span style="color:gray;font-weight:bold;">1192.15</span>, min: 1013.50, avg: 1227.01, max: 1314.85, mdev: 4186.65, cnt:  20
<span style="color:green;font-weight:bold;">                  Elixir</span>: top: <span style="color:gray;font-weight:bold;">1247.25</span>, min: 1212.52, avg: 1322.45, max: 1615.08, mdev: 4187.53, cnt:  20
<span style="color:green;font-weight:bold;">                   Julia</span>: top: <span style="color:gray;font-weight:bold;">1270.19</span>, min:  984.37, avg: 1342.39, max: 1462.14, mdev: 4187.93, cnt:  20
<span style="color:green;font-weight:bold;">                    Bash</span>: top: <span style="color:gray;font-weight:bold;">1783.38</span>, min: 1769.13, avg: 1811.78, max: 1913.25, mdev: 3897.83, cnt:  17
<span style="color:green;font-weight:bold;">           Ruby_rubinius</span>: top: <span style="color:gray;font-weight:bold;">1968.79</span>, min: 1864.61, avg: 2019.64, max: 2141.07, mdev: 3842.18, cnt:  15
<span style="color:green;font-weight:bold;">              POSIX_dash</span>: top: <span style="color:gray;font-weight:bold;">2011.66</span>, min: 1955.63, avg: 2145.52, max: 2454.85, mdev: 3810.43, cnt:  14
<span style="color:green;font-weight:bold;">                       R</span>: top: <span style="color:gray;font-weight:bold;">2063.77</span>, min: 2047.02, avg: 2127.87, max: 2529.48, mdev: 3843.32, cnt:  15
<span style="color:green;font-weight:bold;">               POSIX_zsh</span>: top: <span style="color:gray;font-weight:bold;">2197.11</span>, min: 2161.49, avg: 2271.15, max: 2425.82, mdev: 3808.04, cnt:  14
<span style="color:green;font-weight:bold;">           Bash_parallel</span>: top: <span style="color:gray;font-weight:bold;">2242.45</span>, min: 2204.63, avg: 2357.10, max: 2647.63, mdev: 3770.02, cnt:  13
<span style="color:green;font-weight:bold;">              POSIX_bash</span>: top: <span style="color:gray;font-weight:bold;">2427.49</span>, min: 2370.05, avg: 2506.06, max: 2835.54, mdev: 3508.84, cnt:  12
</pre>
