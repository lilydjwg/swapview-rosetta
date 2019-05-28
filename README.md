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

Updated at 2019-05-28:

<pre>
<span style="font-weight:bold;color:green;">           Rust_parallel</span>: top: <span style="font-weight:bold;color:gray;">  31.66</span>, min:   30.25, avg:   34.20, max:   39.98, mdev:    2.93, cnt:  20
<span style="font-weight:bold;color:green;">               C++98_omp</span>: top: <span style="font-weight:bold;color:gray;">  41.75</span>, min:   39.39, avg:   44.87, max:   52.22, mdev:    3.67, cnt:  20
<span style="font-weight:bold;color:green;">            Go_goroutine</span>: top: <span style="font-weight:bold;color:gray;">  43.49</span>, min:   42.13, avg:   45.84, max:   58.78, mdev:    3.81, cnt:  20
<span style="font-weight:bold;color:green;">         Crystal_process</span>: top: <span style="font-weight:bold;color:gray;">  48.93</span>, min:   46.54, avg:   50.96, max:   61.33, mdev:    3.27, cnt:  20
<span style="font-weight:bold;color:green;">                   C++17</span>: top: <span style="font-weight:bold;color:gray;">  77.53</span>, min:   76.20, avg:   78.89, max:   82.55, mdev:    1.62, cnt:  20
<span style="font-weight:bold;color:green;">                   C++98</span>: top: <span style="font-weight:bold;color:gray;">  78.39</span>, min:   78.06, avg:   79.03, max:   81.92, mdev:    0.87, cnt:  20
<span style="font-weight:bold;color:green;">                   C++14</span>: top: <span style="font-weight:bold;color:gray;">  78.46</span>, min:   76.19, avg:   80.56, max:   95.23, mdev:    3.90, cnt:  20
<span style="font-weight:bold;color:green;">             C++14_boost</span>: top: <span style="font-weight:bold;color:gray;">  78.78</span>, min:   76.17, avg:   80.00, max:   85.82, mdev:    1.91, cnt:  20
<span style="font-weight:bold;color:green;">                       C</span>: top: <span style="font-weight:bold;color:gray;">  88.63</span>, min:   85.95, avg:   90.86, max:  106.44, mdev:    4.04, cnt:  20
<span style="font-weight:bold;color:green;">                    Rust</span>: top: <span style="font-weight:bold;color:gray;">  89.41</span>, min:   89.16, avg:   90.22, max:   92.38, mdev:    1.02, cnt:  20
<span style="font-weight:bold;color:green;">                     PHP</span>: top: <span style="font-weight:bold;color:gray;">  91.57</span>, min:   90.19, avg:   93.31, max:   99.70, mdev:    2.49, cnt:  20
<span style="font-weight:bold;color:green;">                      Go</span>: top: <span style="font-weight:bold;color:gray;">  91.75</span>, min:   90.19, avg:   93.72, max:  106.00, mdev:    3.44, cnt:  20
<span style="font-weight:bold;color:green;">                   C++11</span>: top: <span style="font-weight:bold;color:gray;">  92.84</span>, min:   89.41, avg:   94.52, max:  104.65, mdev:    2.85, cnt:  20
<span style="font-weight:bold;color:green;">                  Cython</span>: top: <span style="font-weight:bold;color:gray;"> 109.84</span>, min:  107.11, avg:  111.28, max:  117.36, mdev:    2.16, cnt:  20
<span style="font-weight:bold;color:green;">                 Crystal</span>: top: <span style="font-weight:bold;color:gray;"> 115.18</span>, min:  113.91, avg:  116.68, max:  121.05, mdev:    1.81, cnt:  20
<span style="font-weight:bold;color:green;">           Crystal_fiber</span>: top: <span style="font-weight:bold;color:gray;"> 120.10</span>, min:  117.47, avg:  122.22, max:  128.93, mdev:    2.88, cnt:  20
<span style="font-weight:bold;color:green;">                     Nim</span>: top: <span style="font-weight:bold;color:gray;"> 122.51</span>, min:  119.30, avg:  123.52, max:  125.16, mdev:    1.45, cnt:  20
<span style="font-weight:bold;color:green;">                   OCaml</span>: top: <span style="font-weight:bold;color:gray;"> 132.44</span>, min:  129.44, avg:  135.86, max:  141.06, mdev:    3.89, cnt:  20
<span style="font-weight:bold;color:green;">              D_parallel</span>: top: <span style="font-weight:bold;color:gray;"> 135.69</span>, min:  134.71, avg:  138.15, max:  146.49, mdev:    3.25, cnt:  20
<span style="font-weight:bold;color:green;">         D_parallel_llvm</span>: top: <span style="font-weight:bold;color:gray;"> 136.67</span>, min:  133.96, avg:  138.74, max:  143.62, mdev:    2.40, cnt:  20
<span style="font-weight:bold;color:green;">              Python3_mp</span>: top: <span style="font-weight:bold;color:gray;"> 142.48</span>, min:  140.12, avg:  145.28, max:  154.64, mdev:    3.55, cnt:  20
<span style="font-weight:bold;color:green;">                  LuaJIT</span>: top: <span style="font-weight:bold;color:gray;"> 142.57</span>, min:  138.48, avg:  145.08, max:  150.07, mdev:    3.05, cnt:  20
<span style="font-weight:bold;color:green;">                  D_llvm</span>: top: <span style="font-weight:bold;color:gray;"> 144.32</span>, min:  140.51, avg:  146.66, max:  152.88, mdev:    2.95, cnt:  20
<span style="font-weight:bold;color:green;">                 Python2</span>: top: <span style="font-weight:bold;color:gray;"> 144.37</span>, min:  142.10, avg:  147.11, max:  153.80, mdev:    3.30, cnt:  20
<span style="font-weight:bold;color:green;">                       D</span>: top: <span style="font-weight:bold;color:gray;"> 156.35</span>, min:  155.47, avg:  157.50, max:  161.66, mdev:    1.52, cnt:  20
<span style="font-weight:bold;color:green;">                    Vala</span>: top: <span style="font-weight:bold;color:gray;"> 162.35</span>, min:  160.47, avg:  166.09, max:  183.56, mdev:    5.25, cnt:  20
<span style="font-weight:bold;color:green;">                  NodeJS</span>: top: <span style="font-weight:bold;color:gray;"> 173.62</span>, min:  171.94, avg:  176.15, max:  185.79, mdev:    3.55, cnt:  20
<span style="font-weight:bold;color:green;">                    PyPy</span>: top: <span style="font-weight:bold;color:gray;"> 177.34</span>, min:  174.59, avg:  181.06, max:  192.67, mdev:    4.71, cnt:  20
<span style="font-weight:bold;color:green;">                   Lua51</span>: top: <span style="font-weight:bold;color:gray;"> 178.01</span>, min:  174.86, avg:  180.70, max:  187.21, mdev:    3.27, cnt:  20
<span style="font-weight:bold;color:green;">                 Python3</span>: top: <span style="font-weight:bold;color:gray;"> 181.95</span>, min:  177.89, avg:  185.85, max:  202.93, mdev:    5.38, cnt:  20
<span style="font-weight:bold;color:green;">           Python3_bytes</span>: top: <span style="font-weight:bold;color:gray;"> 183.76</span>, min:  178.57, avg:  187.50, max:  198.51, mdev:    4.76, cnt:  20
<span style="font-weight:bold;color:green;">                   Lua52</span>: top: <span style="font-weight:bold;color:gray;"> 184.64</span>, min:  182.37, avg:  188.47, max:  198.35, mdev:    4.40, cnt:  20
<span style="font-weight:bold;color:green;">                  Erlang</span>: top: <span style="font-weight:bold;color:gray;"> 189.55</span>, min:  173.88, avg:  214.64, max:  265.47, mdev:   29.23, cnt:  20
<span style="font-weight:bold;color:green;">                   Lua53</span>: top: <span style="font-weight:bold;color:gray;"> 195.85</span>, min:  193.45, avg:  199.02, max:  214.06, mdev:    4.45, cnt:  20
<span style="font-weight:bold;color:green;">                    Perl</span>: top: <span style="font-weight:bold;color:gray;"> 198.40</span>, min:  196.03, avg:  202.55, max:  223.36, mdev:    5.94, cnt:  20
<span style="font-weight:bold;color:green;">              FreePascal</span>: top: <span style="font-weight:bold;color:gray;"> 235.26</span>, min:  232.95, avg:  237.63, max:  246.81, mdev:    3.13, cnt:  20
<span style="font-weight:bold;color:green;">          NodeJS_cluster</span>: top: <span style="font-weight:bold;color:gray;"> 241.52</span>, min:  235.63, avg:  246.54, max:  256.75, mdev:    6.25, cnt:  20
<span style="font-weight:bold;color:green;">                    Ruby</span>: top: <span style="font-weight:bold;color:gray;"> 248.76</span>, min:  244.17, avg:  252.83, max:  265.90, mdev:    5.09, cnt:  20
<span style="font-weight:bold;color:green;">                   Guile</span>: top: <span style="font-weight:bold;color:gray;"> 289.66</span>, min:  284.63, avg:  295.38, max:  314.62, mdev:    7.27, cnt:  20
<span style="font-weight:bold;color:green;">            CoffeeScript</span>: top: <span style="font-weight:bold;color:gray;"> 294.49</span>, min:  260.60, avg:  307.95, max:  339.71, mdev:   18.64, cnt:  20
<span style="font-weight:bold;color:green;">                    Java</span>: top: <span style="font-weight:bold;color:gray;"> 304.58</span>, min:  301.25, avg:  309.49, max:  335.23, mdev:    7.17, cnt:  20
<span style="font-weight:bold;color:green;">             PyPy3_bytes</span>: top: <span style="font-weight:bold;color:gray;"> 314.29</span>, min:  308.44, avg:  317.11, max:  328.49, mdev:    4.42, cnt:  20
<span style="font-weight:bold;color:green;">            NodeJS_async</span>: top: <span style="font-weight:bold;color:gray;"> 319.20</span>, min:  304.10, avg:  330.01, max:  365.63, mdev:   14.50, cnt:  20
<span style="font-weight:bold;color:green;">   CoffeeScript_parallel</span>: top: <span style="font-weight:bold;color:gray;"> 322.12</span>, min:  313.18, avg:  332.55, max:  353.75, mdev:   12.65, cnt:  20
<span style="font-weight:bold;color:green;">                     Tcl</span>: top: <span style="font-weight:bold;color:gray;"> 416.50</span>, min:  411.25, avg:  420.01, max:  430.85, mdev:    4.61, cnt:  20
<span style="font-weight:bold;color:green;">                  CSharp</span>: top: <span style="font-weight:bold;color:gray;"> 493.34</span>, min:  487.85, avg:  497.50, max:  504.82, mdev:    5.05, cnt:  20
<span style="font-weight:bold;color:green;">                   PyPy3</span>: top: <span style="font-weight:bold;color:gray;"> 497.04</span>, min:  487.73, avg:  502.10, max:  524.44, mdev:    7.98, cnt:  20
<span style="font-weight:bold;color:green;">                   Julia</span>: top: <span style="font-weight:bold;color:gray;"> 513.74</span>, min:  509.61, avg:  519.10, max:  549.11, mdev:    8.53, cnt:  20
<span style="font-weight:bold;color:green;">          CommonLisp_opt</span>: top: <span style="font-weight:bold;color:gray;"> 612.56</span>, min:  609.27, avg:  617.61, max:  628.88, mdev:    5.84, cnt:  20
<span style="font-weight:bold;color:green;">          CommonLisp_old</span>: top: <span style="font-weight:bold;color:gray;"> 613.03</span>, min:  605.48, avg:  620.37, max:  638.00, mdev:    9.03, cnt:  20
<span style="font-weight:bold;color:green;">                  Racket</span>: top: <span style="font-weight:bold;color:gray;"> 687.28</span>, min:  681.27, avg:  697.35, max:  731.37, mdev:   13.68, cnt:  20
<span style="font-weight:bold;color:green;">         Racket_compiled</span>: top: <span style="font-weight:bold;color:gray;"> 692.82</span>, min:  682.87, avg:  700.38, max:  712.80, mdev:    8.98, cnt:  20
<span style="font-weight:bold;color:green;">                  Elixir</span>: top: <span style="font-weight:bold;color:gray;"> 740.82</span>, min:  732.09, avg:  748.43, max:  766.95, mdev:    9.57, cnt:  20
<span style="font-weight:bold;color:green;">                   Scala</span>: top: <span style="font-weight:bold;color:gray;"> 762.85</span>, min:  752.22, avg:  774.40, max:  804.77, mdev:   14.08, cnt:  20
<span style="font-weight:bold;color:green;">                       R</span>: top: <span style="font-weight:bold;color:gray;">1025.07</span>, min: 1020.76, avg: 1032.81, max: 1066.16, mdev: 4186.23, cnt:  20
<span style="font-weight:bold;color:green;">           Bash_parallel</span>: top: <span style="font-weight:bold;color:gray;">1044.99</span>, min: 1031.45, avg: 1057.94, max: 1084.40, mdev: 4186.24, cnt:  20
<span style="font-weight:bold;color:green;">              POSIX_dash</span>: top: <span style="font-weight:bold;color:gray;">1235.55</span>, min: 1228.05, avg: 1257.52, max: 1357.65, mdev: 4186.37, cnt:  20
<span style="font-weight:bold;color:green;">                 Haskell</span>: top: <span style="font-weight:bold;color:gray;">1237.07</span>, min: 1230.71, avg: 1249.83, max: 1283.97, mdev: 4186.25, cnt:  20
<span style="font-weight:bold;color:green;">               POSIX_zsh</span>: top: <span style="font-weight:bold;color:gray;">1430.97</span>, min: 1410.50, avg: 1475.44, max: 1782.56, mdev: 4075.51, cnt:  20
<span style="font-weight:bold;color:green;">              POSIX_bash</span>: top: <span style="font-weight:bold;color:gray;">1434.86</span>, min: 1421.93, avg: 1451.54, max: 1513.64, mdev: 4074.64, cnt:  20
<span style="font-weight:bold;color:green;">                    Bash</span>: top: <span style="font-weight:bold;color:gray;">1821.54</span>, min: 1758.91, avg: 1900.82, max: 2326.75, mdev: 3873.64, cnt:  16
<span style="font-weight:bold;color:red;">              ChezScheme</span>: FAILED with entity not found
<span style="font-weight:bold;color:red;">                 Chicken</span>: FAILED with entity not found
<span style="font-weight:bold;color:red;">                    Dart</span>: FAILED with entity not found
<span style="font-weight:bold;color:red;">                Haskell2</span>: FAILED with entity not found
<span style="font-weight:bold;color:red;">               OCaml_lwt</span>: FAILED with entity not found
<span style="font-weight:bold;color:red;">           Ruby_rubinius</span>: FAILED with entity not found
</pre>

ChezScheme and Ruby_rubinius are not included because no binary packages available for Arch Linux and the others are [broken](https://github.com/lilydjwg/swapview/tree/master/broken).
