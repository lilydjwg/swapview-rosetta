CC=ghc
CFLAGS=-O2

.PHONY: all clean

%: %.hs
	$(CC) $(CFLAGS) $<
	strip $@

all: swapview

clean:
	-rm -f *.o *.hi swapview
