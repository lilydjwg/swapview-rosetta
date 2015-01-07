CC=rustc

.PHONY: all clean

%: %.rs
	$(CC) -O -C lto $<
	strip $@

all: swapview

clean:
	-rm -f swapview
