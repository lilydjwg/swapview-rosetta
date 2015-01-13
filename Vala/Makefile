default: swapview

swapview: swapview.vala
	valac --pkg gio-2.0 --Xcc=-O4 $^ -o $@
	strip swapview

clean:
	-rm -f swapview
