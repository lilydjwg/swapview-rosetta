default: swapview

swapview: swapview.d
	dmd -O -release -inline -boundscheck=off ./swapview.d
	strip $@

swapview-llvm: swapview
	ldmd -O -release -inline -boundscheck=off ./swapview.d
	mv swapview swapview-llvm
	strip swapview-llvm


clean:
	-rm -f swapview{,.o}

