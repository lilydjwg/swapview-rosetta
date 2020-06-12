CXXFLAGS=-O4 -flto -Wall -fno-exceptions -fno-rtti -std=c++14
CXX=g++

swapview: swapview.cpp
	$(CXX) swapview.cpp -o swapview $(CXXFLAGS)
	strip swapview

clean:
	-rm -f swapview
