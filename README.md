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
* C++17
* CSharp (mono)
* ChezScheme
* Chicken, format (will be installed by `make`)
* CommonLisp_opt, sbcl
* CommonLisp_old, sbcl, maynbe others also work
* CoffeeScript, requires promise (will be installed by `make`)
* CoffeeScript_parallel, a parallel version, requires promise (will be installed by `make`)
* Crystal, with fiber and multi-process versions, tested with 0.28.0
* Cython
* D, `dmd` or `ldmd` (LLVM version)
* Dart
* Elixir
* Erlang
* Go >=1.8
* Guile >= 2.1
* Haskell, requires `haskell-strict`
* Haskell2, another better Haskell version using more dependencies
* Java, >= Java 8
* Lua, requires lua-filesystem, works with 5.1, 5.2, 5.3 and LuaJIT
* Nim (tested with 0.17.0)
* NodeJS, requires sprintf (will be installed by `make`)
* NodeJS_async, another NodeJS version which use async I/O, requires sprintf and async (will be installed by `make`)
* NodeJS_cluster, uses multiple forked worker process via Node Cluster API
* OCaml
* FreePascal
* Perl
* Python, works with Python 2 & 3
* Python3_bytes
* R
* Racket
* Ruby and Rubinius
* Rust, >= 1.20
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

This is tested on Arch Linux, with latest versions of compilers and interpreters available there at testing time. CPU is Intel(R) Core(TM) i7-7700HQ.

Updated at 2017-07-14:

<pre>
<span style="color:green;font-weight:bold;">           Rust_parallel</span>: top: <span style="color:gray;font-weight:bold;">  30.48</span>, min:   27.76, avg:   32.48, max:   37.80, mdev:    2.78, cnt:  20
<span style="color:green;font-weight:bold;">               C++98_omp</span>: top: <span style="color:gray;font-weight:bold;">  31.24</span>, min:   29.04, avg:   34.42, max:   49.48, mdev:    4.52, cnt:  20
<span style="color:green;font-weight:bold;">            Go_goroutine</span>: top: <span style="color:gray;font-weight:bold;">  68.30</span>, min:   61.87, avg:   75.89, max:  142.91, mdev:   16.39, cnt:  20
<span style="color:green;font-weight:bold;">                   C++14</span>: top: <span style="color:gray;font-weight:bold;">  83.17</span>, min:   82.23, avg:   84.71, max:   92.58, mdev:    2.76, cnt:  20
<span style="color:green;font-weight:bold;">             C++14_boost</span>: top: <span style="color:gray;font-weight:bold;">  83.58</span>, min:   83.20, avg:   84.58, max:   91.00, mdev:    1.72, cnt:  20
<span style="color:green;font-weight:bold;">                   C++98</span>: top: <span style="color:gray;font-weight:bold;">  83.71</span>, min:   83.09, avg:   85.19, max:   91.48, mdev:    2.44, cnt:  20
<span style="color:green;font-weight:bold;">                    Rust</span>: top: <span style="color:gray;font-weight:bold;">  91.45</span>, min:   90.81, avg:   93.08, max:   99.38, mdev:    2.07, cnt:  20
<span style="color:green;font-weight:bold;">                       C</span>: top: <span style="color:gray;font-weight:bold;">  91.49</span>, min:   90.49, avg:   93.41, max:   99.44, mdev:    2.53, cnt:  20
<span style="color:green;font-weight:bold;">                   C++11</span>: top: <span style="color:gray;font-weight:bold;">  91.81</span>, min:   91.33, avg:   93.52, max:  102.80, mdev:    3.04, cnt:  20
<span style="color:green;font-weight:bold;">                     PHP</span>: top: <span style="color:gray;font-weight:bold;">  93.91</span>, min:   93.37, avg:   94.98, max:   99.42, mdev:    1.47, cnt:  20
<span style="color:green;font-weight:bold;">                   OCaml</span>: top: <span style="color:gray;font-weight:bold;"> 106.85</span>, min:  105.75, avg:  109.34, max:  118.03, mdev:    3.37, cnt:  20
<span style="color:green;font-weight:bold;">                     Nim</span>: top: <span style="color:gray;font-weight:bold;"> 109.28</span>, min:  108.44, avg:  110.75, max:  117.43, mdev:    2.13, cnt:  20
<span style="color:green;font-weight:bold;">         D_parallel_llvm</span>: top: <span style="color:gray;font-weight:bold;"> 111.25</span>, min:  109.43, avg:  113.21, max:  117.26, mdev:    2.33, cnt:  20
<span style="color:green;font-weight:bold;">              D_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 116.77</span>, min:  114.69, avg:  118.95, max:  125.45, mdev:    2.87, cnt:  20
<span style="color:green;font-weight:bold;">                    PyPy</span>: top: <span style="color:gray;font-weight:bold;"> 126.23</span>, min:  124.29, avg:  128.34, max:  134.07, mdev:    2.79, cnt:  20
<span style="color:green;font-weight:bold;">                  D_llvm</span>: top: <span style="color:gray;font-weight:bold;"> 129.63</span>, min:  128.52, avg:  131.32, max:  137.65, mdev:    2.41, cnt:  20
<span style="color:green;font-weight:bold;">                  LuaJIT</span>: top: <span style="color:gray;font-weight:bold;"> 132.68</span>, min:  131.31, avg:  134.36, max:  143.07, mdev:    2.57, cnt:  20
<span style="color:green;font-weight:bold;">                      Go</span>: top: <span style="color:gray;font-weight:bold;"> 135.57</span>, min:  132.37, avg:  139.25, max:  148.37, mdev:    4.50, cnt:  20
<span style="color:green;font-weight:bold;">                       D</span>: top: <span style="color:gray;font-weight:bold;"> 146.30</span>, min:  145.00, avg:  149.14, max:  159.02, mdev:    3.85, cnt:  20
<span style="color:green;font-weight:bold;">                Haskell2</span>: top: <span style="color:gray;font-weight:bold;"> 150.92</span>, min:  149.41, avg:  153.25, max:  164.60, mdev:    3.53, cnt:  20
<span style="color:green;font-weight:bold;">                 Python2</span>: top: <span style="color:gray;font-weight:bold;"> 155.36</span>, min:  152.26, avg:  158.55, max:  170.20, mdev:    4.60, cnt:  20
<span style="color:green;font-weight:bold;">                    Vala</span>: top: <span style="color:gray;font-weight:bold;"> 159.55</span>, min:  157.87, avg:  161.40, max:  166.52, mdev:    2.26, cnt:  20
<span style="color:green;font-weight:bold;">                  Erlang</span>: top: <span style="color:gray;font-weight:bold;"> 163.00</span>, min:  158.63, avg:  168.76, max:  181.77, mdev:    7.09, cnt:  20
<span style="color:green;font-weight:bold;">                   Lua51</span>: top: <span style="color:gray;font-weight:bold;"> 166.58</span>, min:  164.58, avg:  168.89, max:  181.71, mdev:    3.69, cnt:  20
<span style="color:green;font-weight:bold;">                   Lua52</span>: top: <span style="color:gray;font-weight:bold;"> 168.48</span>, min:  167.40, avg:  170.82, max:  178.11, mdev:    3.36, cnt:  20
<span style="color:green;font-weight:bold;">           Python3_bytes</span>: top: <span style="color:gray;font-weight:bold;"> 174.30</span>, min:  172.65, avg:  176.83, max:  181.64, mdev:    2.91, cnt:  20
<span style="color:green;font-weight:bold;">                   Lua53</span>: top: <span style="color:gray;font-weight:bold;"> 180.20</span>, min:  177.79, avg:  185.01, max:  199.41, mdev:    6.07, cnt:  20
<span style="color:green;font-weight:bold;">                    Perl</span>: top: <span style="color:gray;font-weight:bold;"> 180.22</span>, min:  177.30, avg:  182.21, max:  186.09, mdev:    2.44, cnt:  20
<span style="color:green;font-weight:bold;">              FreePascal</span>: top: <span style="color:gray;font-weight:bold;"> 180.85</span>, min:  179.35, avg:  184.23, max:  197.83, mdev:    4.84, cnt:  20
<span style="color:green;font-weight:bold;">                 Python3</span>: top: <span style="color:gray;font-weight:bold;"> 181.72</span>, min:  178.47, avg:  184.09, max:  189.67, mdev:    2.99, cnt:  20
<span style="color:green;font-weight:bold;">                    Ruby</span>: top: <span style="color:gray;font-weight:bold;"> 199.82</span>, min:  197.16, avg:  203.62, max:  218.32, mdev:    4.92, cnt:  20
<span style="color:green;font-weight:bold;">                 Chicken</span>: top: <span style="color:gray;font-weight:bold;"> 234.69</span>, min:  232.11, avg:  239.61, max:  248.39, mdev:    5.63, cnt:  20
<span style="color:green;font-weight:bold;">             PyPy3_bytes</span>: top: <span style="color:gray;font-weight:bold;"> 238.55</span>, min:  237.18, avg:  242.08, max:  253.68, mdev:    4.53, cnt:  20
<span style="color:green;font-weight:bold;">                   Guile</span>: top: <span style="color:gray;font-weight:bold;"> 254.49</span>, min:  249.14, avg:  260.40, max:  275.83, mdev:    7.12, cnt:  20
<span style="color:green;font-weight:bold;">              ChezScheme</span>: top: <span style="color:gray;font-weight:bold;"> 265.63</span>, min:  262.52, avg:  268.56, max:  278.53, mdev:    3.94, cnt:  20
<span style="color:green;font-weight:bold;">                    Java</span>: top: <span style="color:gray;font-weight:bold;"> 291.35</span>, min:  283.94, avg:  302.36, max:  324.82, mdev:   12.38, cnt:  20
<span style="color:green;font-weight:bold;">                  NodeJS</span>: top: <span style="color:gray;font-weight:bold;"> 317.01</span>, min:  314.61, avg:  321.04, max:  332.05, mdev:    4.71, cnt:  20
<span style="color:green;font-weight:bold;">                    Dart</span>: top: <span style="color:gray;font-weight:bold;"> 329.39</span>, min:  325.63, avg:  334.57, max:  351.19, mdev:    6.92, cnt:  20
<span style="color:green;font-weight:bold;">           Ruby_rubinius</span>: top: <span style="color:gray;font-weight:bold;"> 359.76</span>, min:  357.74, avg:  363.13, max:  373.02, mdev:    4.45, cnt:  20
<span style="color:green;font-weight:bold;">          CommonLisp_opt</span>: top: <span style="color:gray;font-weight:bold;"> 360.57</span>, min:  358.41, avg:  365.15, max:  378.44, mdev:    5.76, cnt:  20
<span style="color:green;font-weight:bold;">                     Tcl</span>: top: <span style="color:gray;font-weight:bold;"> 367.38</span>, min:  363.28, avg:  372.89, max:  388.57, mdev:    6.65, cnt:  20
<span style="color:green;font-weight:bold;">          CommonLisp_old</span>: top: <span style="color:gray;font-weight:bold;"> 376.27</span>, min:  371.99, avg:  379.66, max:  390.55, mdev:    4.33, cnt:  20
<span style="color:green;font-weight:bold;">                   PyPy3</span>: top: <span style="color:gray;font-weight:bold;"> 384.12</span>, min:  376.60, avg:  390.16, max:  401.39, mdev:    7.32, cnt:  20
<span style="color:green;font-weight:bold;">            CoffeeScript</span>: top: <span style="color:gray;font-weight:bold;"> 414.40</span>, min:  393.13, avg:  432.25, max:  466.42, mdev:   20.64, cnt:  20
<span style="color:green;font-weight:bold;">   CoffeeScript_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 451.12</span>, min:  425.11, avg:  464.92, max:  491.52, mdev:   17.05, cnt:  20
<span style="color:green;font-weight:bold;">            NodeJS_async</span>: top: <span style="color:gray;font-weight:bold;"> 454.78</span>, min:  437.13, avg:  465.18, max:  489.06, mdev:   13.02, cnt:  20
<span style="color:green;font-weight:bold;">         Racket_compiled</span>: top: <span style="color:gray;font-weight:bold;"> 510.97</span>, min:  505.22, avg:  516.20, max:  527.69, mdev:    6.23, cnt:  20
<span style="color:green;font-weight:bold;">                  Racket</span>: top: <span style="color:gray;font-weight:bold;"> 520.70</span>, min:  515.11, avg:  525.28, max:  533.79, mdev:    5.87, cnt:  20
<span style="color:green;font-weight:bold;">         NodeJS_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 673.38</span>, min:  664.38, avg:  687.60, max:  724.04, mdev:   16.32, cnt:  20
<span style="color:green;font-weight:bold;">                   Scala</span>: top: <span style="color:gray;font-weight:bold;"> 719.27</span>, min:  698.23, avg:  740.32, max:  815.95, mdev:   27.27, cnt:  20
<span style="color:green;font-weight:bold;">           Bash_parallel</span>: top: <span style="color:gray;font-weight:bold;"> 769.14</span>, min:  751.56, avg:  775.91, max:  791.40, mdev:    8.82, cnt:  20
<span style="color:green;font-weight:bold;">                 Haskell</span>: top: <span style="color:gray;font-weight:bold;">1036.33</span>, min: 1013.27, avg: 1048.70, max: 1090.21, mdev: 4186.25, cnt:  20
<span style="color:green;font-weight:bold;">                  Elixir</span>: top: <span style="color:gray;font-weight:bold;">1097.32</span>, min: 1075.24, avg: 1113.36, max: 1144.80, mdev: 4186.26, cnt:  20
<span style="color:green;font-weight:bold;">                       R</span>: top: <span style="color:gray;font-weight:bold;">1141.37</span>, min: 1120.69, avg: 1156.42, max: 1177.79, mdev: 4186.26, cnt:  20
<span style="color:green;font-weight:bold;">                    Bash</span>: top: <span style="color:gray;font-weight:bold;">1368.00</span>, min: 1323.22, avg: 1479.66, max: 1994.19, mdev: 4077.71, cnt:  20
<span style="color:green;font-weight:bold;">              POSIX_dash</span>: top: <span style="color:gray;font-weight:bold;">1841.09</span>, min: 1833.25, avg: 1851.09, max: 1881.68, mdev: 3897.64, cnt:  17
<span style="color:green;font-weight:bold;">               POSIX_zsh</span>: top: <span style="color:gray;font-weight:bold;">2124.79</span>, min: 2110.81, avg: 2134.32, max: 2156.40, mdev: 3841.56, cnt:  15
<span style="color:green;font-weight:bold;">              POSIX_bash</span>: top: <span style="color:gray;font-weight:bold;">2200.64</span>, min: 2195.09, avg: 2206.75, max: 2221.41, mdev: 3807.09, cnt:  14
<span style="color:red;font-weight:bold;">                  CSharp</span>: FAILED with entity not found
<span style="color:red;font-weight:bold;">                   Julia</span>: FAILED with entity not found
</pre>
