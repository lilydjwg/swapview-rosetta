.PHONY: all clean

%: %.go
	go build $<
	strip $@

all: swapview

clean:
	-rm -f swapview
