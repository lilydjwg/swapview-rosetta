CPPFLAGS=-O4 -Wall -lboost_filesystem -lboost_system -fno-rtti
CPPC=g++

swapview: swapview.cpp
	$(CPPC) --std=c++14 swapview.cpp -o swapview $(CPPFLAGS)
	strip swapview

clean:
	-rm -f swapview
