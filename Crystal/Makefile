CRYSTAL_BIN ?= $(shell which crystal)

.PHONY: all clean

%: %.cr
	$(CRYSTAL_BIN) build --release --no-debug $<
	strip $@

all: swapview swapview_fiber swapview_process

clean:
	-rm -f swapview swapview_fiber swapview_process
