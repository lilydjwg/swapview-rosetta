default: swapview

swapview: swapview.d
	dmd -O -release -inline -boundscheck=off ./swapview.d

clean:
	-rm -f swapview{,.o}

