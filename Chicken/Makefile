default: swapview

install: format.so loops.so

format.so loops.so: libraries.scm
	chicken-install format loops -deploy -p $(PWD)

swapview: swapview.scm install
	csc swapview.scm

clean:
	-rm -f swapview *.so *.setup-info *.o
