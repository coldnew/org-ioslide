(require 'ansi)

(require 'ecukes-reporter)

(defvar ecukes-reporter-counter 0
  "Counter for how many scenarios that has been printed.")

(defvar ecukes-reporter-dot-string "\u22C5"
  "Dots shown after plane.")

(defvar ecukes-reporter-plane-string "\u2708"
  "The plane.")

(add-hook 'ecukes-reporter-start-hook
          (lambda (stats)
            (let ((scenarios (cdr (assoc 'scenarios stats))))
              (unless (zerop scenarios)
                (ecukes-reporter-print (ansi-apply 90 (s-repeat (1+ scenarios) "-")))
                (ecukes-reporter-print-newline)
                (ecukes-reporter-print-newline)
                (ecukes-reporter-print (ansi-apply 90 (s-repeat (1+ scenarios) "-")))
                (ecukes-reporter-print (ansi-backward (1+ scenarios)))
                (ecukes-reporter-print (ansi-up))))))

(add-hook 'ecukes-reporter-before-scenario-hook
          (lambda (scenario)
            (unless (zerop ecukes-reporter-counter)
              (ecukes-reporter-print (ansi-up))
              (ecukes-reporter-print (ansi-forward ecukes-reporter-counter)))
            (setq ecukes-reporter-counter (1+ ecukes-reporter-counter))))

(add-hook 'ecukes-reporter-pending-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-cyan ecukes-reporter-dot-string))))

(add-hook 'ecukes-reporter-scenario-passed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-green ecukes-reporter-dot-string))))

(add-hook 'ecukes-reporter-scenario-failed-hook
          (lambda (scenario)
            (ecukes-reporter-print (ansi-red ecukes-reporter-dot-string))))

(add-hook 'ecukes-reporter-after-scenario-hook
          (lambda (scenario)
            (ecukes-reporter-print ecukes-reporter-plane-string)
            (ecukes-reporter-print-newline)))

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-failing-scenarios-summary)
            (ecukes-reporter-print-newline)
            (ecukes-reporter-print-summary stats)))

(provide 'ecukes-reporter-landing)
