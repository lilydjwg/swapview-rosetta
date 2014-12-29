default: swapview

swapview: swapview.scm format/format.scm
	csc -O5 swapview.scm
	strip swapview

format/format.scm:
	chicken-install -r format 

clean:
	-rm -f swapview *.so *.setup-info *.o .libraries.stamp
	-rm -rf format
