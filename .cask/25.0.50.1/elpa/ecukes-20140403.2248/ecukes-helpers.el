;;; ecukes-helpers.el --- Misc helpers

(require 'dash)
(require 'ecukes-def)

(defun ecukes-feature-steps (ecukes-features)
  "Return all steps in all FEATURES."
  (let* ((scenarios
          (-flatten
           (-map
            (lambda (feature)
              (ecukes-feature-scenarios feature)) ecukes-features)))
         (backgrounds
          (-reject
           'null
           (-map
            (lambda (feature)
              (ecukes-feature-background feature)) ecukes-features)))
         (scenario-steps
          (-map
           (lambda (scenario)
             (ecukes-scenario-steps scenario)) scenarios))
         (background-steps
          (-map
           (lambda (background)
             (ecukes-background-steps background)) backgrounds)))
    (-flatten (-concat background-steps scenario-steps))))

(defun ecukes-format-quote (string)
  "Quote percent signs in STRING."
  (s-replace "%" "%%" string))

(provide 'ecukes-helpers)

;;; ecukes-helpers.el ends here
