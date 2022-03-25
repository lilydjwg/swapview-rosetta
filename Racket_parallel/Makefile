.PHONY: all clean

all: compiled/swapview_rkt.zo

compiled/swapview_rkt.zo: swapview.rkt
	raco make --no-deps swapview.rkt

run: all
	racket ++lang racket/base -tm compiled/swapview_rkt.zo

clean:
	rm -rf compiled
