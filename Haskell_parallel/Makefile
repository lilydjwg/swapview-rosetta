all: swapview

.PHONY: all clean deps cleandeps

swapview: swapview.hs
	stack build
	ln -sf .stack-work/install/x86_64-linux-tinfo6/lts-13.26/8.6.5/bin/swapview
	strip swapview

clean:
	stack clean
	rm swapview
