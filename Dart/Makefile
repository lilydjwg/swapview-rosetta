.packages:
	pub get

swapview: swapview.dart .packages
	dart2native swapview.dart -o swapview

all: swapview-native

clean:
	rm -rf .dart_tool/ packages/ .packages pubspec.lock swapview
