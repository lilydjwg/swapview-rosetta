default: install swapview

CHECKEN_INSTALL_REPOSITORY:=$(shell pwd)/deps
CHICKEN_REPOSITORY_PATH:=$(shell pwd)/deps:/usr/lib/chicken/9
export CHECKEN_INSTALL_REPOSITORY
export CHECKEN_REPOSITORY_PATH

install:
	mkdir -p $(CHICKEN_INSTALL_REPOSITORY)
	chicken-install format
	chicken-install srfi-1

swapview: install swapview.scm
	chicken-csc -O5 swapview.scm -include-path $(CHICKEN_INSTALL_REPOSITORY)
	strip swapview

clean:
	-rm -f swapview
	-rm -r deps
