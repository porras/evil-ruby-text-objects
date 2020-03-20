CASK ?= cask

default: build

build: test
	${CASK} build

test:
	${CASK} exec ert-runner --quiet

deps:
	${CASK} install

clean-elc:
	${CASK} clean-elc

clean: clean-elc
	rm -rf .cask

check: clean-elc
	${CASK} emacs -Q --batch \
	--eval "(require 'package)" \
	--eval "(push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)" \
	--eval "(package-initialize)" \
	--eval "(package-refresh-contents)" \
	-l elisp-lint.el \
	-f elisp-lint-files-batch evil-ruby-text-objects.el

.PHONY: test
