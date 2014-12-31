.PHONY: all clean

all: compiled/swapview_rkt.zo

compiled/swapview_rkt.zo: swapview.rkt
	raco make --no-deps swapview.rkt

run: all
	racket compiled/swapview_rkt.zo

clean:
	-rm -rf compiled
