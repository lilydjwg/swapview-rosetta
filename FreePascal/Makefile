FPFLAGS=-O3
FPC=fpc

swapview: swapview.pas
	$(FPC) swapview.pas $(FPFLAGS)
	strip swapview

clean:
	-rm -f swapview{,.o}
