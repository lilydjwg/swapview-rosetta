all: swapview swapview-par swapview-lam swapview-par-lam

CXXFLAGS = -O4 -flto -Wall -fno-exceptions -fno-rtti -std=c++17 -lstdc++fs
CXX ?= g++

PAR = -DPARALLEL -ltbb
LAM = -DLAMBDA

swapview: swapview.cpp
	$(CXX) swapview.cpp -o swapview $(CXXFLAGS)
	strip swapview

swapview-par: swapview.cpp
	$(CXX) swapview.cpp -o swapview-par $(PAR) $(CXXFLAGS)
	strip swapview

swapview-lam: swapview.cpp
	$(CXX) swapview.cpp -o swapview-lam $(LAM) $(CXXFLAGS)
	strip swapview

swapview-par-lam: swapview.cpp
	$(CXX) swapview.cpp -o swapview-par-lam $(PAR) $(LAM) $(CXXFLAGS)
	strip swapview

.PHONY: all clean

clean:
	-rm -f swapview
