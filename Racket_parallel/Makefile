.PHONY: all clean

all: compiled/swapview_rkt.zo

compiled/swapview_rkt.zo: swapview.rkt
	raco make swapview.rkt

run: all
	racket -tm compiled/swapview_rkt.zo

clean:
	rm -rf compiled
