default: node_modules/.timestamp

node_modules/.timestamp: package.json
	npm install && coffee -bc *.coffee && touch node_modules/.timestamp

clean:
	-rm -rf node_modules

run: node_modules
	node swapview.js
