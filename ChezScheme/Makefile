default: swapview.so

swapview.so: swapview.ss
	echo '(compile-program "swapview.ss")' | chez-scheme -q --optimize-level 3


clean:
	-rm -f swapview.so
