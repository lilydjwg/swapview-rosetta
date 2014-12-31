default: swapview.js node_modules/.timestamp

%.js: %.coffee
	coffee -bc $<

node_modules/.timestamp: package.json
	npm install && touch node_modules/.timestamp

clean:
	-rm -rf node_modules *.js

run: node_modules
	node swapview.js
