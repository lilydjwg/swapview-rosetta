all: swapview

swapview: swapview.lisp
	sbcl --noinform --quit --eval "(progn (load (compile-file \"swapview.lisp\"))(save-lisp-and-die \"swapview\" :toplevel #'main :executable t))"

sprof: swapview.lisp
	sbcl --noinform --quit --eval "(progn (require :sb-sprof) (load (compile-file \"swapview.lisp\")))"

clean:
	-rm -f swapview *.fasl *.fas *.lib


