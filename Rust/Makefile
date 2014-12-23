CC=rustc

.PHONY: all clean

%: %.rs
	$(CC) -O $<
	strip $@

all: swapview
	
clean:
	-rm -f swapview
