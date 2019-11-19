all: swapview swapview_omp

CXXFLAGS=-O4 -flto -Wall -fno-exceptions -fno-rtti -std=c++98
CXX=g++

swapview: swapview.cpp
	$(CXX) $^ -o $@ $(CXXFLAGS)
	strip $@

swapview_omp: swapview.cpp
	$(CXX) -DUSE_OMP -fopenmp $^ -o $@ $(CXXFLAGS)
	strip $@

.PHONY: all clean

clean:
	-rm -f swapview{,_omp}
