

.PHONY: install test repl

repl:
	dyna

test:
	pytest

install:
	pip install -r requirements.txt
	python setup.py develop
