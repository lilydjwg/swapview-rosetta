TARGET = swapview.beam
all:: $(TARGET)

swapview.beam: swapview.erl
	erlc $^ -o $@

test:: all
	./erlswapview

clean::
	-rm -f $(TARGET)
