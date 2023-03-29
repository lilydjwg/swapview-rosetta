.PHONY: all clean

all: swapview_rkt_merged.zo

swapview_rkt_merged.zo: swapview.rkt
	raco demod swapview.rkt

run: all
	racket swapview_rkt_merged.zo

clean:
	-rm -rf compiled swapview_rkt_merged.zo
