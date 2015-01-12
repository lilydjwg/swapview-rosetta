MONOFLAGS=--desktop 
MONO=mono-sgen

default: SwapView.exe.so

SwapView.exe: SwapView.cs
	mcs -clscheck- -debug- -optimize+ SwapView.cs

SwapView.exe.so: SwapView.exe
	$(MONO) --aot -O=all,-shared $(MONOFLAGS) SwapView.exe

clean:
	-rm -f SwapView.exe{,.so}

run: SwapView.exe.so
	$(MONO) $(MONOFLAGS) SwapView.exe
