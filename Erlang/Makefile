TARGET = swapview.beam line_server.beam
all:: $(TARGET)

swapview.beam: swapview.erl
	erlc $^

line_server.beam: line_server.erl
	erlc $^

test:: all
	./erlswapview

clean::
	-rm -f $(TARGET) erl_crash.dump
