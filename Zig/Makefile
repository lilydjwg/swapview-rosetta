.PHONY: all clean

%: %.zig
	zig build-exe -Drelease-safe=true --strip $<

all: swapview

clean:
	-rm -f swapview
	-rm -rf zig-cache

test:
	zig test swapview.zig
