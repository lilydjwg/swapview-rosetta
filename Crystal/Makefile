CRYSTAL_BIN ?= $(shell which crystal)

.PHONY: all clean

%: %.cr
	$(CRYSTAL_BIN) build --release --no-debug $<
	strip $@

all: swapview

clean:
	-rm -f swapview
