.PHONY: all clean

%: %.zig
	zig build-exe -O ReleaseSafe --strip $<

all: swapview

clean:
	-rm -f swapview
	-rm -rf zig-cache

test:
	zig test swapview.zig
