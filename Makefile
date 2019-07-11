CASK ?= cask

default: build

build: test
	${CASK} build

test:
	${CASK} exec ert-runner --quiet

deps:
	${CASK} install

clean:
	${CASK} clean-elc
	rm -rf .cask

.PHONY: test
