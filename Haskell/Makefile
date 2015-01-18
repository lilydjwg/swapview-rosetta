all: swapview

.PHONY: all clean deps cleandeps

swapview: .cabal-sandbox/.timestamp swapview.hs
	cabal install --reinstall
	strip .cabal-sandbox/bin/swapview
	ln -s .cabal-sandbox/bin/swapview swapview

deps: .cabal-sandbox/.timestamp

.cabal-sandbox/.timestamp:
	cabal sandbox init && cabal install --only-dependencies && touch .cabal-sandbox/.timestamp

clean:
	-rm -f *.o *.hi swapview .cabal-sandbox/bin/swapview

cleandeps: clean
	-cabal sandbox delete
	-rm -fr dist
