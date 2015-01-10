all: swapview swapview-llvm

swapview: swapview.d
	dmd -O -release -inline -boundscheck=off $<
	strip $@

swapview-llvm: swapview.d
	ldmd -O -release -inline -boundscheck=off $< -of$@
	strip $@

clean:
	-rm -f swapview{,-llvm} *.o

