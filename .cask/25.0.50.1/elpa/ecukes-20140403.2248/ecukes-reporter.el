;;; ecukes-reporter.el --- generic reporter interface and helper functions

(require 'f)
(require 's)
(require 'dash)
(require 'ansi)

(require 'ecukes-core)
(require 'ecukes-def)
(require 'ecukes-steps)
(require 'ecukes-template)


;;;; Variables and constants

(eval-when-compile
  (defvar ecukes-path)
  (defvar ecukes-failing-scenarios-file))

(defconst ecukes-reporters
  '((gangsta  . "gangsta talk")
    (landing  . "landing plane")
    (magnars  . "@magnars stripped spec")
    (progress . "progress bar")
    (spec     . "full blown spec")
    (dot      . "one colored dot per scenario (default)"))
  "List of available reporters, with description.")

(defconst ecukes-reporters-path
  (f-expand "reporters" ecukes-path)
  "Path to reporters directory.")


;;;; Hooks

(defvar ecukes-reporter-start-hook nil
  "Called before anything runs.")

(defvar ecukes-reporter-end-hook nil
  "Called when everything has run, with stats as argument.

The stats alist contains these slots:

 - `scenarios' total number of scenarios
 - `scenarios-passed' number of passed scenarios
 - `scenarios-failed' number of failed scenarios
 - `steps' total number of steps
 - `steps-passed' number of passed steps
 - `steps-failed' number of failed steps
 - `steps-skipped' number of skipped steps")

(defvar ecukes-reporter-before-first-feature-hook nil
  "Called before first feature runs, with feature as argument.")

(defvar ecukes-reporter-before-last-feature-hook nil
  "Called before last feature runs, with feature as argument.")

(defvar ecukes-reporter-before-feature-hook nil
  "Called before feature runs with, feature as argument.")

(defvar ecukes-reporter-after-first-feature-hook nil
  "Called after first feature runs, with feature as argument.")

(defvar ecukes-reporter-after-last-feature-hook nil
  "Called after last feature runs, with feature as argument..")

(defvar ecukes-reporter-after-feature-hook nil
  "Called after feature runs, with feature as argument.")

(defvar ecukes-reporter-before-first-scenario-hook nil
  "Called before first scenario runs, with scenario as argument.")

(defvar ecukes-reporter-before-last-scenario-hook nil
  "Called before last scenario runs, with scenario as argument.")

(defvar ecukes-reporter-before-scenario-hook nil
  "Called before scenario runs, with scenario as argument.")

(defvar ecukes-reporter-after-first-scenario-hook nil
  "Called before after first scenario runs, with scenario as argument.")

(defvar ecukes-reporter-after-last-scenario-hook nil
  "Called before after last scenario runs, with scenario as argument.")

(defvar ecukes-reporter-after-scenario-hook nil
  "Called before after scenario runs, with scenario as argument.")

(defvar ecukes-reporter-scenario-passed-hook nil
  "Called when scenario passed, with scenario as argument.")

(defvar ecukes-reporter-scenario-failed-hook nil
  "Called when scenario failed, with scenario as argument.")

(defvar ecukes-reporter-pending-scenario-hook nil
  "Called for pending scenarios.

That is scenarios that will not run becuase of for example some
pattern, anti-pattern or tags.")

(defvar ecukes-reporter-before-first-step-hook nil
  "Called before first step runs, with step and status as arguments.")

(defvar ecukes-reporter-before-last-step-hook nil
  "Called before last step runs, with step and status as arguments.")

(defvar ecukes-reporter-before-step-hook nil
  "Called before step runs, with step and status as arguments.")

(defvar ecukes-reporter-after-first-step-hook nil
  "Called before after first step runs, with step and status as arguments.")

(defvar ecukes-reporter-after-last-step-hook nil
  "Called before after last step runs, with step and status as arguments.")

(defvar ecukes-reporter-after-step-hook nil
  "Called before after step runs, with step and status as arguments.")

(defvar ecukes-reporter-after-step-success-hook nil
  "Called after step success, with step as argument.")

(defvar ecukes-reporter-after-step-failed-hook nil
  "Called after step failed, with step as argument.")

(defvar ecukes-reporter-after-step-skipped-hook nil
  "Called after step skipped, with step as argument.")

(defvar ecukes-reporter-after-step-hook nil
  "Called after step, with step as argument.")

(defvar ecukes-reporter-before-background-hook nil
  "Called before backgrund runs.")

(defvar ecukes-reporter-after-background-hook nil
  "Called after backgrund runs.")

(defvar ecukes-reporter-steps-without-definition-hook nil
  "...")


;;;; Internal helpers

(defun ecukes-reporter-valid? (reporter)
  "Return if REPORTER is valid or not."
  (let ((reporters (--map (symbol-name (car it)) ecukes-reporters)))
    (-contains? reporters reporter)))

(defvar ecukes-reporter-failed-scenarios nil
  "List of failing scenarios.")

(add-hook 'ecukes-reporter-scenario-failed-hook
          (lambda (scenario)
            (add-to-list 'ecukes-reporter-failed-scenarios scenario t)))


;;;; Core functions

(defun ecukes-reporter-use (reporter)
  "Use REPORTER."
  (unless (ecukes-reporter-valid? reporter)
    (ecukes-fail "Invalid reporter: %s" reporter))
  (let ((full-reporter (format "ecukes-reporter-%s" reporter)))
    (require (intern full-reporter)
             (f-expand full-reporter ecukes-reporters-path))))

(defun ecukes-reporter-print (&rest args)
  "Print message.

If first message is an integer, indent with that amount of
whitespace before the actual text content is printed.

The rest of the arguments will be applied to `format'."
  (let (indent format-string objects)
    (cond ((stringp (car args))
           (setq indent 0)
           (setq format-string (car args))
           (setq objects (cdr args)))
          (:else
           (setq indent (car args))
           (setq format-string (nth 1 args))
           (setq objects (-drop 2 args))))
    (let ((ecukes-message t))
      (princ (s-concat (s-repeat indent " ") (apply 'format (cons format-string objects)))))))

(defun ecukes-reporter-println (&rest args)
  "Like `ecukes-reporter-print' but also prints a newline."
  (apply 'ecukes-reporter-print args)
  (ecukes-reporter-print-newline))

(defun ecukes-reporter-print-newline ()
  "Print newline."
  (ecukes-reporter-print "\n"))


;;;; Summary

(defun ecukes-reporter-print-scenarios-summary (stats)
  "Print scenario summary."
  (let ((scenarios (cdr (assoc 'scenarios stats))))
    (ecukes-reporter-print
     (if (zerop scenarios)
         "0 scenarios"
       (format
        "%d scenarios (%s, %s)"
        scenarios
        (ansi-red "%d failed" (cdr (assoc 'scenarios-failed stats)))
        (ansi-green "%d passed" (cdr (assoc 'scenarios-passed stats))))))))

(defun ecukes-reporter-print-steps-summary (stats)
  "Print step summary."
  (let ((steps (cdr (assoc 'steps stats))))
    (ecukes-reporter-print
     (if (zerop steps)
         "0 steps"
       (format
        "%d steps (%s, %s, %s)"
        steps
        (ansi-red "%d failed" (cdr (assoc 'steps-failed stats)))
        (ansi-cyan "%d skipped" (cdr (assoc 'steps-skipped stats)))
        (ansi-green "%d passed" (cdr (assoc 'steps-passed stats))))))))

(defun ecukes-reporter-print-summary (stats)
  "Print summary of STATS."
  (ecukes-reporter-print-scenarios-summary stats)
  (ecukes-reporter-print-newline)
  (ecukes-reporter-print-steps-summary stats)
  (ecukes-reporter-print-newline))


;;;; Spec style print functions

(defun ecukes-reporter-print-feature-header (feature)
  "Print FEATURE and description if any."
  (-when-let (intro (ecukes-feature-intro feature))
    (let* ((header (ecukes-intro-header intro))
           (description (ecukes-intro-description intro))
           (scenarios (ecukes-feature-scenarios feature)))
      (ecukes-reporter-println "Feature: %s" header)
      (--each description (ecukes-reporter-println 2 it))
      (when description
        (ecukes-reporter-print-newline)))))

(defun ecukes-reporter-print-background-header ()
  "Print background header."
  (ecukes-reporter-println 2 "Background:"))

(defun ecukes-reporter-print-scenario-header (scenario &optional color)
  "Print SCENARIO header."
  (let* ((name (ecukes-scenario-name scenario))
         (tags (ecukes-scenario-tags scenario))
         (header (format "Scenario: %s" name))
         (header (if color (ansi-apply color header) header)))
    (when tags
      (let ((tags-string (ansi-cyan (s-join " " (--map (s-concat "@" it) tags)))))
        (ecukes-reporter-println 2 tags-string)))
    (ecukes-reporter-println 2 header)))

(defun ecukes-reporter-print-table (step)
  "Print STEP table."
  (let* (widths
         (table (ecukes-step-arg step))
         (rows (length table))
         (cols (length (car table))))
    (-dotimes
     cols
     (lambda (col)
       (let ((width 0))
         (-dotimes
          rows
          (lambda (row)
            (setq width (max width (length (nth col (nth row table)))))))
         (!cons width widths))))
    (setq widths (reverse widths))
    (-dotimes
     rows
     (lambda (row)
       (let (col-strings)
         (-dotimes
          cols
          (lambda (col)
            (let* ((orig (nth col (nth row table)))
                   (pad (- (nth col widths) (length orig)))
                   (col-string (s-concat orig (s-repeat pad " "))))
              (push col-string col-strings))))
         (ecukes-reporter-println 6 "| %s |" (s-join " | " (nreverse col-strings))))))))

(defun ecukes-reporter-print-py-string (step)
  "Print STEP py-string."
  (ecukes-reporter-println 6 "\"\"\"")
  (let ((lines (s-lines (ecukes-step-arg step))))
    (--each lines (ecukes-reporter-println 6 it)))
  (ecukes-reporter-println 6 "\"\"\""))

(defun ecukes-reporter-print-step (step)
  "Print STEP."
  (let* ((name (ecukes-step-name step))
         (type (ecukes-step-type step))
         (status (ecukes-step-status step))
         (color
          (cond ((eq status 'success) 'ansi-green)
                ((eq status 'failure) 'ansi-red)
                ((eq status 'skipped) 'ansi-cyan))))
    (ecukes-reporter-println 4 (funcall color name))
    (when (eq type 'table)
      (ecukes-reporter-print-table step))
    (when (eq type 'py-string)
      (ecukes-reporter-print-py-string step))
    (let ((err (ecukes-step-err step)))
      (when err
        (--each (s-lines err)
          (ecukes-reporter-println 6 (ansi-red it)))))))

(defun ecukes-reporter-print-failing-scenarios-summary ()
  "Print a summary of failing scenarios."
  (when ecukes-reporter-failed-scenarios
    (-each
     ecukes-reporter-failed-scenarios
     (lambda (scenario)
       (let ((steps (ecukes-scenario-steps scenario)))
         (ecukes-reporter-print-newline)
         (ecukes-reporter-print-scenario-header scenario)
         (-each steps 'ecukes-reporter-print-step))))))


;;;; Missing steps

(defun ecukes-reporter-print-missing-steps (steps)
  "Print missing steps"
  (ecukes-reporter-println
   (ansi-yellow "Please implement the following step definitions"))
  (ecukes-reporter-print-newline)
  (let (step-bodies)
    (-each
     steps
     (lambda (step)
       (let ((step-body (ecukes-reporter--step-body step))
             (step-string (ecukes-reporter--step-string step)))
         (unless
             (--any? (equal step-body it) step-bodies)
           (push step-body step-bodies)
           (ecukes-reporter-println step-string)))))))

(defun ecukes-reporter--step-string (step)
  "Return missing step string."
  (let ((head (ecukes-step-head step))
        (body (ecukes-reporter--step-body step))
        (args (ecukes-reporter--step-args step)))
    (ansi-yellow
     (ecukes-template-get
      'missing-step
      `(("head" . ,head)
        ("body" . ,body)
        ("args" . ,args))))))

(defun ecukes-reporter--step-args (step)
  "Return args from STEP."
  (let* (result
         (arg (ecukes-step-arg step))
         (args (ecukes-steps-args step))
         (type (ecukes-step-type step))
         (args-count
          (+
           (length args)
           (if (or
                (equal type 'table)
                (equal type 'py-string)) 1 0))))
    (if (= args-count 1)
        "arg"
      (progn
        (-dotimes
         args-count
         (lambda (n)
           (push (format "arg-%d" (1+ n)) result)))
        (s-join " " (nreverse result))))))

(defun ecukes-reporter--step-body (step)
  "Return body from STEP."
  (let* ((body (ecukes-step-body step))
         (args (ecukes-steps-args step))
         (result body))
    (when args
      (-each
       args
       (lambda (arg)
         (setq result (s-replace (s-concat "\"" arg "\"") "\\\"\\\\([^\\\"]+\\\\)\\\"" result)))))
    result))


(add-hook 'ecukes-reporter-steps-without-definition-hook
          (lambda (steps)
            (ecukes-reporter-print-missing-steps steps)))


;;;; Printing step definitions

(defun ecukes-reporter-print-steps (&optional with-doc with-file)
  "Print all available steps defined for this project.
Include docstring when WITH-DOC is non-nil."
  (-each
   ecukes-steps-definitions
   (lambda (step-def)
     (let (row)
       (when with-file
         (let ((file (ecukes-step-file-name step-def t)))
           (setq row (s-concat row file ": "))))
       (let ((regex (ecukes-step-def-regex step-def)))
         (setq row (s-concat row (ansi-green regex))))
       (when with-doc
         (let ((doc (ecukes-step-def-doc step-def)))
           (when doc
             (setq row (s-concat row "\n" (ansi-cyan doc) "\n")))))
       (ecukes-reporter-println row)))))


;;;; Save list of failed scenarios to file

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (if ecukes-reporter-failed-scenarios
                (let* ((scenario-names
                        (-map
                         (lambda (scenario)
                           (s-downcase (ecukes-scenario-name scenario)))
                         ecukes-reporter-failed-scenarios))
                       (lines (s-join "\n" scenario-names)))
                  (f-write-text lines 'utf-8 ecukes-failing-scenarios-file))
              (when (f-file? ecukes-failing-scenarios-file)
                (f-delete ecukes-failing-scenarios-file)))))

(provide 'ecukes-reporter)

;;; ecukes-reporter.el ends here
