.PHONY: all clean

%: %.go
	go build -ldflags "-w -s" $<

all: swapview

clean:
	-rm -f swapview
