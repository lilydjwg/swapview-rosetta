all: swapview-opt

swapview-opt: swapview-opt.lisp
	sbcl --noinform --quit --eval "(progn (load (compile-file \"swapview-opt.lisp\"))(save-lisp-and-die \"swapview-opt\" :toplevel #'main :executable t))"

sprof: swapview-opt.lisp
	sbcl --noinform --quit --eval "(progn (require :sb-sprof) (load (compile-file \"swapview-opt.lisp\")))"

clean:
	-rm -f swapview-opt *.fasl *.fas


