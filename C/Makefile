CFLAGS:=-O4 -Wall -std=c11 
CC:=gcc
EXT:=
STRIP:=strip

BIN=swapview$(EXT)

$(BIN): swapview.c
	$(CC) swapview.c -o $(BIN) $(CFLAGS)
	$(STRIP) $(BIN)

clean:
	-rm -f $(BIN)
