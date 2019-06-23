all: swapview

.PHONY: all clean deps cleandeps

swapview: .cabal-sandbox/.timestamp swapview.hs
	cabal v1-install --reinstall --ghc-options -dynamic
	strip .cabal-sandbox/bin/swapview
	ln -sf .cabal-sandbox/bin/swapview swapview

deps: .cabal-sandbox/.timestamp

.cabal-sandbox/.timestamp:
	cabal v1-sandbox init && cabal v1-install --only-dependencies && touch .cabal-sandbox/.timestamp

clean:
	-rm -f *.o *.hi swapview .cabal-sandbox/bin/swapview

cleandeps: clean
	-cabal v1-sandbox delete
	-rm -fr dist
