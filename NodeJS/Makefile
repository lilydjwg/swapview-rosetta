default: run

node_modules: package.json
	npm install

clean:
	-rm -rf node_modules

run: node_modules
	node swapview.js
