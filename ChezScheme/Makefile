default: swapview.so

swapview.so: swapview.ss
	echo '(compile-program "swapview.ss")' | chez-scheme -q --optimize-level 3

run: swapview.so
	chez-scheme --program swapview.so

clean:
	-rm -f swapview.so
