CC=ghc
CFLAGS=-i$(HOME)/scripts/haskell/lib -O2

.PHONY: all clean

%: %.hs
	$(CC) $(CFLAGS) $<
	strip $@

all: swapview
	
# swapview2 needs text-format

clean:
	-rm -f *.o *.hi swapview swapview2
