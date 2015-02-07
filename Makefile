EMACS ?= emacs
CASK ?= cask
BATCH := $(EMACS) $(EFLAGS) -batch -q -no-site-file -L .

all: compile

test: clean
	${MAKE} all
	${MAKE} unit
unit:
	${CASK} exec ert-runner

clean:
	$(RM) *.elc

compile:
	${CASK} exec  ${EMACS} -Q -batch -f batch-byte-compile ox-ioslide.el

.PHONY: all test unit compile
