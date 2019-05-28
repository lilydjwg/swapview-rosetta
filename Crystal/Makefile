CRYSTAL_BIN ?= $(shell which crystal)
SHARDS_BIN ?= $(shell which shards)

.PHONY: all clean

default: all

%: %.cr
	$(CRYSTAL_BIN) build --release --no-debug $<
	strip $@

lib/.shard_installed: swapview_process.cr
	$(SHARDS_BIN) install
	touch lib/.shard_installed

swapview_process: swapview_process.cr lib/.shard_installed

all: swapview swapview_fiber swapview_process

clean:
	-rm -rf swapview{,_fiber,_process} lib/
