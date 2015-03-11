TARGET = swapview.beam
all:: $(TARGET)

swapview.beam: swapview.erl
	erlc $^

test:: all
	./erlswapview

clean::
	-rm -f $(TARGET) erl_crash.dump
