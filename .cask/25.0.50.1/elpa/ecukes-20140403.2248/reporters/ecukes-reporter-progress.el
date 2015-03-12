(require 'ansi)

(require 'ecukes-reporter)
(require 'ecukes-helpers)

(defconst ecukes-reporter-dot-string "\u00B7"
  "Dot string.")

(defvar ecukes-reporter-to-end 0
  "How many columns to end position.")

(defvar ecukes-reporter-total-scenarios 0
  "Total number of scenarios.")

(defvar ecukes-reporter-scenarios-count 0
  "Currently running scenario count.")

(defun ecukes-reporter--inc ()
  (setq ecukes-reporter-to-end (1- ecukes-reporter-to-end))
  (setq ecukes-reporter-scenarios-count (1+ ecukes-reporter-scenarios-count)))

(add-hook 'ecukes-reporter-start-hook
          (lambda (stats)
            (setq ecukes-reporter-total-scenarios (cdr (assoc 'scenarios stats)))
            (ecukes-reporter-print "[%s] " (s-repeat ecukes-reporter-total-scenarios " "))
            (ecukes-reporter-print "0%%")
            (setq ecukes-reporter-to-end (+ 4 ecukes-reporter-total-scenarios))
            (ecukes-reporter-print (ansi-backward ecukes-reporter-to-end))))

(add-hook 'ecukes-reporter-pending-scenario-hook
          (lambda (scenario)
            (ecukes-reporter--inc)
            (ecukes-reporter-print (ansi-cyan ecukes-reporter-dot-string))))

(add-hook 'ecukes-reporter-scenario-passed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-green ecukes-reporter-dot-string))))

(add-hook 'ecukes-reporter-scenario-failed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-red "x"))))

(add-hook 'ecukes-reporter-before-scenario-hook
          (lambda (scenario)
            (unless (zerop ecukes-reporter-scenarios-count)
              (ecukes-reporter-print (ansi-up))
              (ecukes-reporter-print (ansi-forward (1+ ecukes-reporter-scenarios-count))))))

(add-hook 'ecukes-reporter-after-scenario-hook
          (lambda (scenario)
            (ecukes-reporter--inc)
            (let ((progress-string (format "%d%%" (* 100 (/ (* ecukes-reporter-scenarios-count 1.0) ecukes-reporter-total-scenarios)))))
              (ecukes-reporter-print (ansi-forward (- ecukes-reporter-to-end 2)))
              (ecukes-reporter-print (ecukes-format-quote progress-string))
              (ecukes-reporter-print (ansi-backward (+ (- (length progress-string) 2) ecukes-reporter-to-end))))
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (ecukes-reporter-print-failing-scenarios-summary)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-summary stats)))

(provide 'ecukes-reporter-progress)
