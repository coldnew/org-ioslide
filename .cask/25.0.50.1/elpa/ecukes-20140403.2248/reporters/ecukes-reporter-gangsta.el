;;; ecukes-reporter-gangsta.el --- Gangsta talk reporter

;; NOTE: Someone with more street cred than me should probably look
;; this over.

(require 'ansi)

(require 'ecukes-core)
(require 'ecukes-reporter)


;;;; Variables

(defconst ecukes-reporter-successful-functions
  '(ecukes-reporter-smoked-white-boys)
  "List of successful functions.")

(defconst ecukes-reporter-failed-functions
  '(ecukes-reporter-white-boys-still-livin)
  "List of failed functions.")


;;;; Functions for successful and failing tests

(defun ecukes-reporter-smoked-white-boys (stats)
  (format
   "Y'all niggas, I's smoked all of them %d white boys $"
   (cdr (assoc 'scenarios stats))))

(defun ecukes-reporter-white-boys-still-livin (stats)
  (format
   "You's not worth daugshit homeboy. %d of %d white boys still livin'."
   (cdr (assoc 'scenarios-failed stats))
   (cdr (assoc 'scenarios stats))))


;;;; Hookin' it ap daug

(add-hook 'ecukes-reporter-end-hook
          (lambda (stats)
            (let ((scenarios-failed (cdr (assoc 'scenarios-failed stats))))
              (ecukes-reporter-println
               (cond ((zerop scenarios-failed)
                      (ansi-green
                       (let ((i (random (length ecukes-reporter-successful-functions))))
                         (funcall (nth i ecukes-reporter-successful-functions) stats))))
                     (:else
                      (ansi-red
                       (let ((i (random (length ecukes-reporter-failed-functions))))
                         (funcall (nth i ecukes-reporter-failed-functions) stats))))))
              (ecukes-reporter-print-failing-scenarios-summary)
              (ecukes-reporter-print-newline)
              (ecukes-reporter-print-summary stats))))



(provide 'ecukes-reporter-gangsta)

;;; ecukes-reporter-gangsta.el ends here
