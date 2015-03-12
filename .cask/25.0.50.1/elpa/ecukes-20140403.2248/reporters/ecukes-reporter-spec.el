;;; ecukes-reporter-spec.el --- Full blown spec

(require 's)
(require 'dash)

(require 'ecukes-reporter)

(add-hook 'ecukes-reporter-before-feature-hook
          (lambda (feature)
            (ecukes-reporter-print-feature-header feature)))

(add-hook 'ecukes-reporter-before-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print-scenario-header scenario)))

(add-hook 'ecukes-reporter-before-background-hook
          (lambda ()
            (ecukes-reporter-print-background-header)))

(add-hook 'ecukes-reporter-after-background-hook
          (lambda ()
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-after-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-after-step-hook
          (lambda (step status)
            (ecukes-reporter-print-step step)))

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (ecukes-reporter-print-summary stats)))

(provide 'ecukes-reporter-spec)

;;; ecukes-reporter-spec.el ends here
