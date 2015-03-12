;;; ecukes-reporter-dot.el --- One dot per scenario reporter

(require 'ansi)

(require 'ecukes-reporter)

(defvar ecukes-reporter-dot-counter 0
  "Counter for how many dots have been printed.")

(add-hook 'ecukes-reporter-scenario-passed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-green "."))))

(add-hook 'ecukes-reporter-scenario-failed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-red "."))))

(add-hook 'ecukes-reporter-before-scenario-hook
          (lambda (scenario)
            (unless (zerop ecukes-reporter-dot-counter)
              (ecukes-reporter-print (ansi-up))
              (ecukes-reporter-print (ansi-forward ecukes-reporter-dot-counter)))
            (setq ecukes-reporter-dot-counter (1+ ecukes-reporter-dot-counter))))

(add-hook 'ecukes-reporter-after-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (ecukes-reporter-print-failing-scenarios-summary)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-summary stats)))

(provide 'ecukes-reporter-dot)

;;; ecukes-reporter-dot.el ends here
