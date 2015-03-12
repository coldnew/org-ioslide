;;; ecukes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ecukes-steps" "ecukes-steps.el" (21717 58244
;;;;;;  0 0))
;;; Generated autoloads from ecukes-steps.el

(defalias 'Given 'ecukes-steps-define-or-call-step "\
Put the system in a known state.")

(defalias 'When 'ecukes-steps-define-or-call-step "\
Describe the key action.")

(defalias 'Then 'ecukes-steps-define-or-call-step "\
Observe outcomes.")

(defalias 'And 'ecukes-steps-define-or-call-step "\
Make Given/When/Then read more fluently.")

(defalias 'But 'ecukes-steps-define-or-call-step "\
Make Given/When/Then read more fluently.")

(autoload 'ecukes-steps-define-or-call-step "ecukes-steps" "\
Define or call step.

When *defining* a step, argument takes the following form:
    (STEP-REGEXP [DOC] FUNCTION)
where STEP-REGEXP is a regular expression defining a step and
FUNCTION is the definition of the step.  You can optionally
give a docstring DOC as the second argument.

When *calling* a step, argument takes the following form:
    (STEP-NAME [ARG [ARG ..]])

\(fn STEP-REGEXP [DOC] FUNCTION | STEP-NAME &optional ARGS)" nil nil)

(put 'ecukes-steps-define-or-call-step 'lisp-indent-function 'defun)

(put 'ecukes-steps-define-or-call-step 'doc-string-elt 2)

;;;***

;;;### (autoloads nil nil ("ecukes-byte-compile.el" "ecukes-cli.el"
;;;;;;  "ecukes-core.el" "ecukes-def.el" "ecukes-helpers.el" "ecukes-hooks.el"
;;;;;;  "ecukes-load.el" "ecukes-new.el" "ecukes-parse.el" "ecukes-pkg.el"
;;;;;;  "ecukes-project.el" "ecukes-reporter.el" "ecukes-run.el"
;;;;;;  "ecukes-stats.el" "ecukes-template.el" "ecukes.el") (21717
;;;;;;  58244 631757 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ecukes-autoloads.el ends here
