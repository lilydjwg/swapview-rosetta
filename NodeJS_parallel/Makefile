default: node_modules/.timestamp

node_modules/.timestamp: package.json
	npm install && \
	touch node_modules/.timestamp && \
	sed -i -e "s/coreCount =.*/coreCount = `grep -c ^processor /proc/cpuinfo`;/" swapview.js

clean:
	-rm -rf node_modules

run: node_modules/.timestamp
	node swapview.js
