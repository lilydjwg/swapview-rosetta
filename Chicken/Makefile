default: swapview

swapview: swapview.scm .libraries.stamp
	csc -O4 swapview.scm
	strip swapview

.libraries.stamp: libraries.scm
	chicken-install format loops -deploy -p $$(pwd)
	touch .libraries.stamp

clean:
	-rm -f swapview *.so *.setup-info *.o .libraries.stamp
