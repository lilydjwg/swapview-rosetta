default: SwapView.class

SwapView.class: SwapView.java
	javac -g:none SwapView.java

clean:
	-rm -f SwapView.class

run: SwapView.class
	java -dsa -Xmixed -XX:CompileThreshold=4096 -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -XX:+UseAES -XX:+UseSerialGC SwapView
