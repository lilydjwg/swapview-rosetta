default: swapview

install: format.so loops.so

format.so loops.so: libraries.scm
	chicken-install format loops -deploy -p $$(pwd)

swapview: swapview.scm install
	csc -O4 swapview.scm
	strip swapview

clean:
	-rm -f swapview *.so *.setup-info *.o
