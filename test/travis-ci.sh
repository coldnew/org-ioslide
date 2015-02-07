#!/bin/sh

export-example() {
    travis_fold start export-example
    cd $PWD/example
    "$EMACS" -Q -l ../ox-ioslide.el -batch index.org --eval 'org-ioslide-export-to-html'
    travis_fold end export-example
}

byte-compile() {
    travis_fold start byte-compiling
    "$EMACS" -Q -batch --eval '(setq byte-compile-error-on-warn t)' \
             --eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'
    travis_fold end byte-compiling
}

declare -f byte-compile export-example
