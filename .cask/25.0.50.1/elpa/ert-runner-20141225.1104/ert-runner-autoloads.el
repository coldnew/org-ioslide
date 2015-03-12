;;; ert-runner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ert-compat" "ert-compat.el" (21717 58247 0
;;;;;;  0))
;;; Generated autoloads from ert-compat.el

(autoload 'ert-deftest "ert-compat" "\
Define NAME (a symbol) as a test.

BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not' and `should-error' are useful for
assertions in BODY.

Use `ert' to run tests interactively.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] [:tags '(TAG...)] BODY...)" nil (quote macro))

(put 'ert-deftest 'lisp-indent-function 2)

(put 'ert-info 'lisp-indent-function 1)

(autoload 'ert-run-tests-batch "ert-compat" "\
Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object.

\(fn &optional SELECTOR)" nil nil)

(autoload 'ert-run-tests-batch-and-exit "ert-compat" "\
Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the framework detected an error
outside of the tests (e.g. invalid SELECTOR or bug in the code
that runs the tests).

\(fn &optional SELECTOR)" nil nil)

(autoload 'ert-run-tests-interactively "ert-compat" "\
Run the tests specified by SELECTOR and display the results in a buffer.

SELECTOR works as described in `ert-select-tests'.
OUTPUT-BUFFER-NAME and MESSAGE-FN should normally be nil; they
are used for automated self-tests and specify which buffer to use
and how to display message.

\(fn SELECTOR &optional OUTPUT-BUFFER-NAME MESSAGE-FN)" t nil)

(defalias 'ert 'ert-run-tests-interactively)

(autoload 'ert-describe-test "ert-compat" "\
Display the documentation for TEST-OR-TEST-NAME (a symbol or ert-test).

\(fn TEST-OR-TEST-NAME)" t nil)

;;;***

;;;### (autoloads nil nil ("ert-runner-pkg.el" "ert-runner.el") (21717
;;;;;;  58247 925620 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ert-runner-autoloads.el ends here
