CPPFLAGS = -O4 -flto -Wall -fno-exceptions -fno-rtti -std=c++17 -lstdc++fs
CXX ?= g++

PAR = -DPARALLEL -ltbb
LAM = -DLAMBDA

swapview: swapview.cpp
	$(CXX) swapview.cpp -o swapview $(CPPFLAGS)
	strip swapview

swapview-par: swapview.cpp
	$(CXX) swapview.cpp -o swapview-par $(PAR) $(CPPFLAGS)
	strip swapview

swapview-lam: swapview.cpp
	$(CXX) swapview.cpp -o swapview-lam $(LAM) $(CPPFLAGS)
	strip swapview

swapview-par-lam: swapview.cpp
	$(CXX) swapview.cpp -o swapview-par-lam $(PAR) $(LAM) $(CPPFLAGS)
	strip swapview


clean:
	-rm -f swapview
