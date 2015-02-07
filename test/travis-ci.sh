#!/bin/sh

byte-compile() {
    travis_fold start byte-compiling
    "$EMACS" -Q -batch --eval '(setq byte-compile-error-on-warn t)' \
             --eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'
    travis_fold end byte-compiling
}

declare -f byte-compile
