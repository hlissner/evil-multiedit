
all:
	@cask

test:
	@cask exec ert-runner -l evil-multiedit.el

clean:
	@rm -rf .cask
	@rm -f *.elc test/*.elc

.PHONY: test
