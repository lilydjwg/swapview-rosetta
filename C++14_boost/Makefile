CPPFLAGS=-O4 -Wall -lboost_filesystem -lboost_system -fno-rtti -std=c++14
CPPC=g++

swapview: swapview.cpp
	$(CPPC) swapview.cpp -o swapview $(CPPFLAGS)
	strip swapview

clean:
	-rm -f swapview
