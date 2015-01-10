all: swapview swapview_omp

CPPFLAGS=-O4 -Wall -fno-exceptions -fno-rtti -std=c++98
CPPC=g++

swapview: swapview.cpp
	$(CPPC) $^ -o $@ $(CPPFLAGS)
	strip $@

swapview_omp: swapview.cpp
	$(CPPC) -DUSE_OMP -fopenmp $^ -o $@ $(CPPFLAGS)
	strip $@

clean:
	-rm -f swapview{,_omp}
