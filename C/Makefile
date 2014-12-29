CFLAGS=-O4 -Wall
CC=gcc

swapview: swapview.c
	$(CC) --std=c11 swapview.c -o swapview $(CFLAGS)
	strip swapview

clean:
	-rm -f swapview
