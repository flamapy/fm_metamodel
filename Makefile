.ONESHELL:

.PHONY: build
build:
	rm -rf dist
	python3 setup.py sdist bdist_wheel

upload-testpypi:
	python3 -m twine upload --repository testpypi dist/*

upload-pypi:
	python3 -m twine upload --repository pypi dist/*

lint:
	echo "To lint this project, make sure that you have installed the core" >&2;
	prospector

mypy:
	echo "To lint this project, make sure that you have installed the core" >&2;
	mypy famapy

test:
	python -m pytest -sv

cov:
	coverage run --source=famapy -m pytest
	coverage report
	coverage html



