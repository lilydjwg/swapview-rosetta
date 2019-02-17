default: cython.timestamp

cython.timestamp:
	python setup.py build_ext --inplace && touch cython.timestamp

run: cython.timestamp
	python -c "import swapview"
