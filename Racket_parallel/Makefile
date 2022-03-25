.PHONY: all clean

all: compiled/swapview_rkt.zo

compiled/swapview_rkt.zo: swapview.rkt
	raco make --no-deps swapview.rkt

run: all
	racket -tm swapview.rkt

clean:
	rm -rf compiled
