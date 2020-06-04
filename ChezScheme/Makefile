default: swapview.so

swapview.so: swapview.ss
	echo '(compile-program "swapview.ss")' | scheme -q --optimize-level 3

run: swapview.so
	scheme --program swapview.so

clean:
	-rm -f swapview.so
