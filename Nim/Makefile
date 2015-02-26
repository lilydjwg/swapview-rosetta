.PHONY: all clean

%: %.nim
	nim c -d:release $<
	strip $@

all: swapview

clean:
	-rm -f swapview
	-rm -rf nimcache
