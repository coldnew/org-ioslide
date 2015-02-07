EMACS ?= emacs
BATCH := $(EMACS) $(EFLAGS) -batch -q -no-site-file -L .

all: ox-ioslide.elc

clean:
	$(RM) *.elc

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

.PHONY: check clea
