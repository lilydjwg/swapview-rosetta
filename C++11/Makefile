CPPFLAGS=-O4 -flto -Wall
CPPC=g++

swapview: swapview.cc
	$(CPPC) --std=c++11 swapview.cc -o swapview $(CPPFLAGS)
	strip swapview

clean:
	-rm -f swapview
