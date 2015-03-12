;;; ecukes-run.el --- Run features, scenarios, steps etc...

(require 'f)
(require 's)
(require 'dash)
(require 'bytecomp)

(require 'ecukes-parse)
(require 'ecukes-steps)
(require 'ecukes-stats)
(require 'ecukes-helpers)
(require 'ecukes-hooks)
(require 'ecukes-reporter)

(eval-when-compile
  (defvar ecukes-include-tags)
  (defvar ecukes-exclude-tags)
  (defvar ecukes-async-timeout)
  (defvar ecukes-patterns)
  (defvar ecukes-anti-patterns)
  (defvar ecukes-only-failing)
  (defvar ecukes-failing-scenarios-file))

(defun ecukes-run (feature-files)
  "Parse and run FEATURE-FILES if no steps are missing."
  (let* ((ecukes-features (-map 'ecukes-parse-feature feature-files))
         (steps-without-definition
          (ecukes-steps-without-definition (ecukes-feature-steps ecukes-features))))
    (cond (steps-without-definition
           (run-hook-with-args 'ecukes-reporter-steps-without-definition-hook steps-without-definition))
          (:else
           (let ((ecukes-features (length ecukes-features))
                 (scenarios (length (-flatten (-map 'ecukes-feature-scenarios ecukes-features)))))
             (run-hook-with-args
              'ecukes-reporter-start-hook
              `((ecukes-features . ,ecukes-features)
                (scenarios . ,scenarios))))
           (ecukes-hooks-run-setup)
           (ecukes-run-features ecukes-features)
           (ecukes-hooks-run-teardown)
           (run-hook-with-args
            'ecukes-reporter-end-hook
            `((scenarios        . ,ecukes-stats-scenarios)
              (scenarios-passed . ,ecukes-stats-scenarios-passed)
              (scenarios-failed . ,ecukes-stats-scenarios-failed)
              (steps            . ,ecukes-stats-steps)
              (steps-passed     . ,ecukes-stats-steps-passed)
              (steps-failed     . ,ecukes-stats-steps-failed)
              (steps-skipped    . ,ecukes-stats-steps-skipped)))))))

(defun ecukes-run-features (ecukes-features)
  "Run FEATURES."
  (-each
   ecukes-features
   (lambda (feature)
     (let ((first (eq (-first-item ecukes-features) feature))
           (last (eq (-last-item ecukes-features) feature)))
       (when first
         (run-hook-with-args 'ecukes-reporter-before-first-feature-hook feature))
       (when last
         (run-hook-with-args 'ecukes-reporter-before-last-feature-hook feature))
       (run-hook-with-args 'ecukes-reporter-before-feature-hook feature)
       (ecukes-run-feature feature)
       (when first
         (run-hook-with-args 'ecukes-reporter-after-first-feature-hook feature))
       (when last
         (run-hook-with-args 'ecukes-reporter-after-last-feature-hook feature))
       (run-hook-with-args 'ecukes-reporter-after-feature-hook feature)))))

(defun ecukes-run-feature (feature)
  "Run FEATURE."
  (when (and ecukes-only-failing (f-file? ecukes-failing-scenarios-file))
    (setq ecukes-patterns
          (-map
           (lambda (line)
             (s-concat "\\`" line "\\'"))
           (s-lines (f-read-text ecukes-failing-scenarios-file 'utf-8))))
    (setq ecukes-anti-patterns nil))
  (let* ((background (ecukes-feature-background feature))
         (scenarios
          (-select
           (lambda (scenario)
             (let ((tags (ecukes-scenario-tags scenario)))
               (and (or (not ecukes-include-tags)
                        (-intersection ecukes-include-tags tags))
                    (not (-intersection ecukes-exclude-tags tags)))))
           (ecukes-feature-scenarios feature)))
         (scenarios
          (if ecukes-patterns
              (-select
               (lambda (scenario)
                 (let ((name (s-downcase (ecukes-scenario-name scenario))))
                   (--any? (s-matches? it name) ecukes-patterns)))
               scenarios)
            scenarios))
         (scenarios
          (if ecukes-anti-patterns
              (-reject
               (lambda (scenario)
                 (let ((name (s-downcase (ecukes-scenario-name scenario))))
                   (--any? (s-matches? it name) ecukes-anti-patterns)))
               scenarios)
            scenarios)))
    (let ((background-success t)
          (background-should-run (not background)))
      (when background
        (ecukes-hooks-run-before)
        (run-hooks 'ecukes-reporter-before-background-hook)
        (setq background-success (ecukes-run-background background))
        (run-hooks 'ecukes-reporter-after-background-hook))
      (-each
       (ecukes-feature-scenarios feature)
       (lambda (scenario)
         (if (-contains? scenarios scenario)
             (let ((first (equal (-first-item scenarios) scenario))
                   (last (equal (-last-item scenarios) scenario)))
               (when background-should-run (ecukes-hooks-run-before))
               (when (and background background-success background-should-run)
                 (ecukes-run-background-steps background))
               (when first
                 (run-hook-with-args 'ecukes-reporter-before-first-scenario-hook scenario))
               (when last
                 (run-hook-with-args 'ecukes-reporter-before-last-scenario-hook scenario))
               (run-hook-with-args 'ecukes-reporter-before-scenario-hook scenario)
               (ecukes-run-scenario scenario background-success)
               (when first
                 (run-hook-with-args 'ecukes-reporter-after-first-scenario-hook scenario))
               (when last
                 (run-hook-with-args 'ecukes-reporter-after-last-scenario-hook scenario))
               (run-hook-with-args 'ecukes-reporter-after-scenario-hook scenario)
               (ecukes-hooks-run-after)
               (setq background-should-run t))
           (run-hook-with-args 'ecukes-reporter-pending-scenario-hook scenario)))))))

(defun ecukes-run-background-steps (background)
  "Run BACKGROUND steps."
  (-each (ecukes-background-steps background) 'ecukes-run-step))

(defun ecukes-run-background (background)
  "Run BACKGROUND."
  (ecukes-run-steps (ecukes-background-steps background) t))

(defun ecukes-run-scenario (scenario background-success)
  "Run SCENARIO."
  (let* ((steps (ecukes-scenario-steps scenario))
         (success (ecukes-run-steps steps background-success)))
    (cond (success
           (ecukes-stats-scenario-pass)
           (run-hook-with-args 'ecukes-reporter-scenario-passed-hook scenario))
          (:else
           (ecukes-stats-scenario-fail)
           (run-hook-with-args 'ecukes-reporter-scenario-failed-hook scenario)))))

(defun ecukes-run-steps (steps success)
  "Run STEPS and return true if all steps were successful, false otherwise."
  (let ((status (if success 'success 'skipped)))
    (-each
     steps
     (lambda (step)
       (let ((first (equal (-first-item steps) step))
             (last (equal (-last-item steps) step)))
         (when first
           (run-hook-with-args 'ecukes-reporter-before-first-step-hook step status))
         (when last
           (run-hook-with-args 'ecukes-reporter-before-last-step-hook step status))
         (run-hook-with-args 'ecukes-reporter-before-step-hook step status)
         (if success
             (progn
               (setq success (ecukes-run-step step))
               (unless success
                 (setq status 'failure)))
           (setq status 'skipped))
         (setf (ecukes-step-status step) status)
         (when first
           (run-hook-with-args 'ecukes-reporter-after-first-step-hook step status))
         (when last
           (run-hook-with-args 'ecukes-reporter-after-last-step-hook step status))
         (cond ((eq status 'success)
                (ecukes-stats-step-pass)
                (run-hook-with-args 'ecukes-reporter-after-step-success-hook step))
               ((eq status 'failure)
                (ecukes-stats-step-fail)
                (run-hook-with-args 'ecukes-reporter-after-step-failed-hook step))
               ((eq status 'skipped)
                (ecukes-stats-step-skip)
                (run-hook-with-args 'ecukes-reporter-after-step-skipped-hook step)))
         (run-hook-with-args 'ecukes-reporter-after-step-hook step status))))
    success))

(defun ecukes-run-step (step)
  "Run STEP and return true if successful, false otherwise."
  (let (success)
    (condition-case err
        (let* ((body (ecukes-step-body step))
               (arg (ecukes-step-arg step))
               (args (ecukes-steps-args step))
               (args (if arg (append args (list arg)) args))
               (step-def (ecukes-steps-find body))
               (fn (ecukes-step-def-fn step-def))
               (fn-args-count
                (if (byte-code-function-p fn)
                    (car (byte-compile-arglist-signature (aref fn 0)))
                  (length
                   (if (listp fn)
                       (cond ((eq (car fn) 'lambda)
                              (cadr fn))
                             ((eq (car fn) 'closure)
                              (nth 2 fn))))))))
          (if (and (not (symbolp fn)) (> fn-args-count (length args)))
              (progn
                (let ((wait t))
                  (add-to-list 'args (lambda (&rest args) (setq wait nil)) t)
                  (apply fn args)
                  (with-timeout
                      (ecukes-async-timeout
                       (error "Did not callback async step within %s seconds" ecukes-async-timeout))
                    (while wait
                      (accept-process-output nil 0.005)))))
            (apply fn args))
          (setq success t))
      (error
       (setf (ecukes-step-err step) (error-message-string err))
       (ecukes-hooks-run-fail))
      (quit)) ;; allow `keyboard-quit' in step definitions
    success))

(provide 'ecukes-run)

;;; ecukes-run.el ends here
