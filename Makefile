

.PHONY: module install test repl

module:
	cd dyna_backend && $(MAKE) all

repl:
	dyna

test:
	pytest

install:
	pip install -r requirements.txt
	python setup.py develop
