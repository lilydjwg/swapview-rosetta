default: swapview

CHICKEN_INSTALL_REPOSITORY:=$(shell pwd)/deps
CHICKEN_REPOSITORY_PATH:=$(shell pwd)/deps:/usr/lib/chicken/9
export CHICKEN_INSTALL_REPOSITORY
export CHICKEN_REPOSITORY_PATH


deps/.timestamp:
	mkdir -p $(CHICKEN_INSTALL_REPOSITORY)
	chicken-install format
	chicken-install srfi-1
	touch deps/.timestamp 

swapview: deps/.timestamp swapview.scm
	chicken-csc -O5 swapview.scm -include-path $(CHICKEN_INSTALL_REPOSITORY) -static
	strip swapview

run: swapview
	./swapview

clean:
	-rm -f swapview swapview.link
	-rm -r deps
