CRYSTAL_BIN ?= $(shell which crystal)
SHARDS_BIN ?= $(shell which shards)

.PHONY: all clean

default: all

%: %.cr
	$(CRYSTAL_BIN) build --release --no-debug $<
	strip $@

shard_install:
	$(SHARDS_BIN) install

swapview_process: shard_install

all: swapview swapview_fiber shard_install swapview_process

clean:
	-rm -rf swapview{,_fiber,_process} lib/
