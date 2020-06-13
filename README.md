swapview is a simple program to view processes' swap usage on Linux. This is intended to be a lilydjwg's version of Hello World program.

How to run the speed comparison
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

* Bash, requires `bc`
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

Updated at 2020-06-13:

<pre>
<span style="font-weight:bold;color:green;">           Rust_parallel</span>: top: <span style="font-weight:bold;color:gray;">  31.48</span>, min:   29.66, avg:   32.83, max:   36.90, mdev:    1.86, cnt:  20
<span style="font-weight:bold;color:green;">               C++98_omp</span>: top: <span style="font-weight:bold;color:gray;">  49.14</span>, min:   45.32, avg:   53.43, max:   72.15, mdev:    6.00, cnt:  20
<span style="font-weight:bold;color:green;">         Crystal_process</span>: top: <span style="font-weight:bold;color:gray;">  65.50</span>, min:   64.67, avg:   69.53, max:   90.86, mdev:    6.32, cnt:  20
<span style="font-weight:bold;color:green;">            Go_goroutine</span>: top: <span style="font-weight:bold;color:gray;">  96.19</span>, min:   80.59, avg:  107.33, max:  152.03, mdev:   15.97, cnt:  20
<span style="font-weight:bold;color:green;">                    Rust</span>: top: <span style="font-weight:bold;color:gray;"> 105.02</span>, min:  104.25, avg:  106.21, max:  109.86, mdev:    1.64, cnt:  20
<span style="font-weight:bold;color:green;">                   C++98</span>: top: <span style="font-weight:bold;color:gray;"> 142.99</span>, min:  141.01, avg:  145.01, max:  151.68, mdev:    2.62, cnt:  20
<span style="font-weight:bold;color:green;">                   C++17</span>: top: <span style="font-weight:bold;color:gray;"> 143.39</span>, min:  141.34, avg:  145.20, max:  149.27, mdev:    2.15, cnt:  20
<span style="font-weight:bold;color:green;">             C++14_boost</span>: top: <span style="font-weight:bold;color:gray;"> 144.02</span>, min:  142.48, avg:  147.36, max:  161.02, mdev:    4.98, cnt:  20
<span style="font-weight:bold;color:green;">                   C++14</span>: top: <span style="font-weight:bold;color:gray;"> 144.63</span>, min:  142.52, avg:  146.46, max:  151.17, mdev:    2.27, cnt:  20
<span style="font-weight:bold;color:green;">                       C</span>: top: <span style="font-weight:bold;color:gray;"> 144.65</span>, min:  141.69, avg:  152.78, max:  191.49, mdev:   11.96, cnt:  20
<span style="font-weight:bold;color:green;">                     PHP</span>: top: <span style="font-weight:bold;color:gray;"> 145.38</span>, min:  143.82, avg:  147.61, max:  156.60, mdev:    3.28, cnt:  20
<span style="font-weight:bold;color:green;">              Python3_mp</span>: top: <span style="font-weight:bold;color:gray;"> 157.63</span>, min:  152.96, avg:  160.75, max:  167.09, mdev:    3.74, cnt:  20
<span style="font-weight:bold;color:green;">                   C++11</span>: top: <span style="font-weight:bold;color:gray;"> 171.00</span>, min:  168.77, avg:  172.47, max:  181.06, mdev:    2.43, cnt:  20
<span style="font-weight:bold;color:green;">                  Cython</span>: top: <span style="font-weight:bold;color:gray;"> 181.13</span>, min:  179.10, avg:  183.45, max:  192.59, mdev:    2.98, cnt:  20
<span style="font-weight:bold;color:green;">                      Go</span>: top: <span style="font-weight:bold;color:gray;"> 193.06</span>, min:  191.68, avg:  195.08, max:  201.55, mdev:    2.64, cnt:  20
<span style="font-weight:bold;color:green;">                    Vala</span>: top: <span style="font-weight:bold;color:gray;"> 196.43</span>, min:  193.41, avg:  200.22, max:  212.19, mdev:    4.70, cnt:  20
<span style="font-weight:bold;color:green;">         D_parallel_llvm</span>: top: <span style="font-weight:bold;color:gray;"> 196.72</span>, min:  191.97, avg:  200.57, max:  213.42, mdev:    5.15, cnt:  20
<span style="font-weight:bold;color:green;">              D_parallel</span>: top: <span style="font-weight:bold;color:gray;"> 205.46</span>, min:  199.73, avg:  208.67, max:  216.93, mdev:    4.29, cnt:  20
<span style="font-weight:bold;color:green;">                 Crystal</span>: top: <span style="font-weight:bold;color:gray;"> 214.76</span>, min:  210.48, avg:  217.29, max:  224.11, mdev:    3.25, cnt:  20
<span style="font-weight:bold;color:green;">           Crystal_fiber</span>: top: <span style="font-weight:bold;color:gray;"> 226.87</span>, min:  225.04, avg:  233.10, max:  272.75, mdev:   12.50, cnt:  20
<span style="font-weight:bold;color:green;">                  LuaJIT</span>: top: <span style="font-weight:bold;color:gray;"> 226.97</span>, min:  224.28, avg:  231.18, max:  240.60, mdev:    4.86, cnt:  20
<span style="font-weight:bold;color:green;">        Haskell_parallel</span>: top: <span style="font-weight:bold;color:gray;"> 227.06</span>, min:  216.14, avg:  235.88, max:  260.16, mdev:   11.77, cnt:  20
<span style="font-weight:bold;color:green;">                   OCaml</span>: top: <span style="font-weight:bold;color:gray;"> 230.78</span>, min:  229.04, avg:  233.81, max:  242.57, mdev:    3.83, cnt:  20
<span style="font-weight:bold;color:green;">                 Python2</span>: top: <span style="font-weight:bold;color:gray;"> 232.30</span>, min:  228.44, avg:  235.14, max:  245.36, mdev:    3.79, cnt:  20
<span style="font-weight:bold;color:green;">                  NodeJS</span>: top: <span style="font-weight:bold;color:gray;"> 235.54</span>, min:  233.22, avg:  239.51, max:  254.48, mdev:    5.11, cnt:  20
<span style="font-weight:bold;color:green;">                    PyPy</span>: top: <span style="font-weight:bold;color:gray;"> 247.96</span>, min:  244.07, avg:  253.04, max:  285.93, mdev:    8.61, cnt:  20
<span style="font-weight:bold;color:green;">                  D_llvm</span>: top: <span style="font-weight:bold;color:gray;"> 249.79</span>, min:  247.05, avg:  259.87, max:  312.30, mdev:   17.22, cnt:  20
<span style="font-weight:bold;color:green;">                 Python3</span>: top: <span style="font-weight:bold;color:gray;"> 255.07</span>, min:  254.20, avg:  260.64, max:  281.31, mdev:    7.91, cnt:  20
<span style="font-weight:bold;color:green;">           Python3_bytes</span>: top: <span style="font-weight:bold;color:gray;"> 257.99</span>, min:  252.36, avg:  265.09, max:  289.54, mdev:   10.23, cnt:  20
<span style="font-weight:bold;color:green;">                       D</span>: top: <span style="font-weight:bold;color:gray;"> 267.79</span>, min:  266.07, avg:  272.18, max:  301.44, mdev:    7.70, cnt:  20
<span style="font-weight:bold;color:green;">                     Nim</span>: top: <span style="font-weight:bold;color:gray;"> 279.93</span>, min:  278.35, avg:  286.19, max:  314.88, mdev:    8.93, cnt:  20
<span style="font-weight:bold;color:green;">                    Ruby</span>: top: <span style="font-weight:bold;color:gray;"> 281.40</span>, min:  278.51, avg:  285.42, max:  296.61, mdev:    5.06, cnt:  20
<span style="font-weight:bold;color:green;">                   Lua51</span>: top: <span style="font-weight:bold;color:gray;"> 286.97</span>, min:  282.82, avg:  292.06, max:  316.65, mdev:    7.19, cnt:  20
<span style="font-weight:bold;color:green;">                  Erlang</span>: top: <span style="font-weight:bold;color:gray;"> 290.70</span>, min:  219.62, avg:  329.77, max:  393.73, mdev:   51.76, cnt:  20
<span style="font-weight:bold;color:green;">                   Lua52</span>: top: <span style="font-weight:bold;color:gray;"> 294.91</span>, min:  290.32, avg:  300.19, max:  322.82, mdev:    7.16, cnt:  20
<span style="font-weight:bold;color:green;">                    Perl</span>: top: <span style="font-weight:bold;color:gray;"> 300.15</span>, min:  296.57, avg:  307.49, max:  323.94, mdev:    8.76, cnt:  20
<span style="font-weight:bold;color:green;">                   Lua53</span>: top: <span style="font-weight:bold;color:gray;"> 309.14</span>, min:  304.81, avg:  314.05, max:  332.84, mdev:    6.66, cnt:  20
<span style="font-weight:bold;color:green;">                 Haskell</span>: top: <span style="font-weight:bold;color:gray;"> 326.48</span>, min:  323.48, avg:  331.62, max:  346.78, mdev:    6.34, cnt:  20
<span style="font-weight:bold;color:green;">                 Chicken</span>: top: <span style="font-weight:bold;color:gray;"> 376.63</span>, min:  370.26, avg:  382.90, max:  426.14, mdev:   11.16, cnt:  20
<span style="font-weight:bold;color:green;">             PyPy3_bytes</span>: top: <span style="font-weight:bold;color:gray;"> 409.92</span>, min:  398.71, avg:  422.03, max:  468.93, mdev:   17.08, cnt:  20
<span style="font-weight:bold;color:green;">              FreePascal</span>: top: <span style="font-weight:bold;color:gray;"> 417.88</span>, min:  415.39, avg:  423.13, max:  453.09, mdev:    8.20, cnt:  20
<span style="font-weight:bold;color:green;">          NodeJS_cluster</span>: top: <span style="font-weight:bold;color:gray;"> 422.85</span>, min:  409.16, avg:  430.56, max:  444.07, mdev:    9.24, cnt:  20
<span style="font-weight:bold;color:green;">            CoffeeScript</span>: top: <span style="font-weight:bold;color:gray;"> 440.48</span>, min:  420.74, avg:  469.86, max:  555.34, mdev:   38.53, cnt:  20
<span style="font-weight:bold;color:green;">              ChezScheme</span>: top: <span style="font-weight:bold;color:gray;"> 454.38</span>, min:  451.57, avg:  459.67, max:  471.25, mdev:    6.15, cnt:  20
<span style="font-weight:bold;color:green;">                   PyPy3</span>: top: <span style="font-weight:bold;color:gray;"> 456.18</span>, min:  449.58, avg:  474.78, max:  521.61, mdev:   22.42, cnt:  20
<span style="font-weight:bold;color:green;">                    Java</span>: top: <span style="font-weight:bold;color:gray;"> 462.47</span>, min:  455.63, avg:  468.49, max:  478.00, mdev:    6.90, cnt:  20
<span style="font-weight:bold;color:green;">                   Guile</span>: top: <span style="font-weight:bold;color:gray;"> 489.58</span>, min:  478.95, avg:  499.54, max:  529.63, mdev:   13.28, cnt:  20
<span style="font-weight:bold;color:green;">   CoffeeScript_parallel</span>: top: <span style="font-weight:bold;color:gray;"> 500.05</span>, min:  481.84, avg:  524.93, max:  588.15, mdev:   30.29, cnt:  20
<span style="font-weight:bold;color:green;">            NodeJS_async</span>: top: <span style="font-weight:bold;color:gray;"> 518.14</span>, min:  484.02, avg:  548.72, max:  619.11, mdev:   36.99, cnt:  20
<span style="font-weight:bold;color:green;">                     Tcl</span>: top: <span style="font-weight:bold;color:gray;"> 519.87</span>, min:  504.80, avg:  529.65, max:  553.50, mdev:   12.06, cnt:  20
<span style="font-weight:bold;color:green;">             Dart_native</span>: top: <span style="font-weight:bold;color:gray;"> 622.26</span>, min:  616.78, avg:  634.25, max:  675.66, mdev:   16.27, cnt:  20
<span style="font-weight:bold;color:green;">          CommonLisp_opt</span>: top: <span style="font-weight:bold;color:gray;"> 643.17</span>, min:  638.69, avg:  654.79, max:  697.46, mdev:   14.71, cnt:  20
<span style="font-weight:bold;color:green;">          CommonLisp_old</span>: top: <span style="font-weight:bold;color:gray;"> 678.22</span>, min:  669.22, avg:  688.15, max:  701.31, mdev:   11.21, cnt:  20
<span style="font-weight:bold;color:green;">                   Julia</span>: top: <span style="font-weight:bold;color:gray;"> 709.08</span>, min:  702.52, avg:  721.52, max:  753.88, mdev:   15.30, cnt:  20
<span style="font-weight:bold;color:green;">         Racket_compiled</span>: top: <span style="font-weight:bold;color:gray;"> 807.49</span>, min:  799.71, avg:  820.90, max:  852.44, mdev:   15.17, cnt:  20
<span style="font-weight:bold;color:green;">                  Racket</span>: top: <span style="font-weight:bold;color:gray;"> 809.93</span>, min:  802.91, avg:  837.35, max:  970.74, mdev:   46.79, cnt:  20
<span style="font-weight:bold;color:green;">                  CSharp</span>: top: <span style="font-weight:bold;color:gray;"> 828.85</span>, min:  819.36, avg:  843.39, max:  905.66, mdev:   19.79, cnt:  20
<span style="font-weight:bold;color:green;">                    Dart</span>: top: <span style="font-weight:bold;color:gray;"> 843.20</span>, min:  829.84, avg:  861.74, max:  949.99, mdev:   27.52, cnt:  20
<span style="font-weight:bold;color:green;">                   Scala</span>: top: <span style="font-weight:bold;color:gray;"> 944.85</span>, min:  897.81, avg:  996.39, max: 1122.12, mdev: 4186.68, cnt:  20
<span style="font-weight:bold;color:green;">                  Elixir</span>: top: <span style="font-weight:bold;color:gray;">1072.99</span>, min: 1054.52, avg: 1096.13, max: 1242.46, mdev: 4186.40, cnt:  20
<span style="font-weight:bold;color:green;">           Bash_parallel</span>: top: <span style="font-weight:bold;color:gray;">1163.78</span>, min: 1133.18, avg: 1219.09, max: 1340.50, mdev: 4186.69, cnt:  20
<span style="font-weight:bold;color:green;">              POSIX_dash</span>: top: <span style="font-weight:bold;color:gray;">1393.27</span>, min: 1370.19, avg: 1412.92, max: 1461.04, mdev: 4074.64, cnt:  20
<span style="font-weight:bold;color:green;">                       R</span>: top: <span style="font-weight:bold;color:gray;">1475.58</span>, min: 1458.35, avg: 1498.45, max: 1583.44, mdev: 4074.68, cnt:  20
<span style="font-weight:bold;color:green;">              POSIX_bash</span>: top: <span style="font-weight:bold;color:gray;">1563.31</span>, min: 1552.28, avg: 1574.53, max: 1605.96, mdev: 4074.59, cnt:  20
<span style="font-weight:bold;color:green;">               POSIX_zsh</span>: top: <span style="font-weight:bold;color:gray;">1593.08</span>, min: 1578.51, avg: 1620.55, max: 1719.31, mdev: 4062.81, cnt:  19
<span style="font-weight:bold;color:green;">                    Bash</span>: top: <span style="font-weight:bold;color:gray;">2350.38</span>, min: 2223.25, avg: 2506.19, max: 2867.53, mdev: 3579.59, cnt:  13
<span style="font-weight:bold;color:green;">              PowerShell</span>: top: <span style="font-weight:bold;color:gray;">3695.72</span>, min: 3680.86, avg: 3755.14, max: 3893.89, mdev: 2148.64, cnt:   8
<span style="font-weight:bold;color:red;">               OCaml_lwt</span>: FAILED with No such file or directory (os error 2)
<span style="font-weight:bold;color:red;">           Ruby_rubinius</span>: FAILED with No such file or directory (os error 2)
</pre>

Java is Oracle JDK. Ruby_rubinius is not included because no packages available for Arch Linux. OCaml_lwt one is [broken](https://github.com/lilydjwg/swapview/tree/master/broken) at the time of running the benchmark.
