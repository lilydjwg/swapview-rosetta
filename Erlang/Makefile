TARGET = swapview.beam
all:: $(TARGET)

swapview.beam: swapview.erl
	erlc $^

test:: all
	./swapview

clean::
	-rm -f $(TARGET) erl_crash.dump
