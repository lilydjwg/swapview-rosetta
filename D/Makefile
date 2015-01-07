default: swapview

swapview: swapview.d
	dmd -O -release -inline -boundscheck=off ./swapview.d
	strip $@

clean:
	-rm -f swapview{,.o}

