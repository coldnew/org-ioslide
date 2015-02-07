EMACS ?= emacs
CASK ?= cask
BATCH := $(EMACS) $(EFLAGS) -batch -q -no-site-file -L .

all: ox-ioslide.elc

test: clean
	${MAKE} all
	${MAKE} unit

unit:
	${CASK} exec ert-runner

clean:
	$(RM) *.elc

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

.PHONY: all test unit
