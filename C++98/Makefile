CPPFLAGS=-O4 -Wall -fno-exceptions -fno-rtti
CPPC=g++

swapview: swapview.cpp
	$(CPPC) --std=c++98 swapview.cpp -o swapview $(CPPFLAGS)
	strip swapview

clean:
	-rm -f swapview
