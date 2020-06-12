CXXFLAGS=-O4 -flto -Wall
CXX=g++

swapview: swapview.cc
	$(CXX) --std=c++11 swapview.cc -o swapview $(CXXFLAGS)
	strip swapview

clean:
	-rm -f swapview
