

.PHONY: install test


test:
	pytest



install:
	pip install -r requirements.txt
	python setup.py develop
