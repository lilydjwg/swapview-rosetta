CPPFLAGS=-O4 -Wall -fno-exceptions -fno-rtti -std=c++17 -lstdc++fs
CPPC=g++

swapview: swapview.cpp
	$(CPPC) swapview.cpp -o swapview $(CPPFLAGS)
	strip swapview

clean:
	-rm -f swapview
