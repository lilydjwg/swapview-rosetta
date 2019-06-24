all: swapview

.PHONY: all clean deps cleandeps

swapview: swapview.hs
	stack build
	ln -sfr $$(stack path --local-install-root)/bin/swapview
	strip swapview

clean:
	stack clean
	rm swapview
