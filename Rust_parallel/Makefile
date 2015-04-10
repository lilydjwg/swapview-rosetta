.PHONY: all clean

%: %.rs
	cd .swapview && cargo build --release
	strip .swapview/target/release/swapview

all: swapview

clean:
	-rm -rf .swapview/target
