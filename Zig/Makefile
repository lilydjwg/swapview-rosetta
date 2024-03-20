.PHONY: all clean

%: %.zig
	zig build-exe -O ReleaseSafe -fstrip $<

all: swapview

clean:
	-rm -f swapview
	-rm -f swapview.o

test:
	zig test swapview.zig
