;;; ecukes-core.el --- Core functionality common to all Ecukes components

(require 'f)
(require 's)
(require 'dash)

(defvar ecukes-message nil
  "If true message is internal Ecukes message, otherwise external.")

(defvar ecukes-verbose t
  "If true, show all message output, otherwise hide.")

(defvar ecukes-internal-message-log nil
  "List with `message' output.")

(defvar ecukes-message-log nil
  "List with `message' output (only from external code).")

(defvar ecukes-debug-callbacks nil
  "List of functions to callback in debugger.")



(defadvice message (around message-around activate)
  (let ((message
         (s-concat
          (if (car (ad-get-args 0))
              (apply 'format (ad-get-args 0))
            "")
          "\n")))
    (unless ecukes-message
      (add-to-list 'ecukes-message-log message t 'eq))
    (when (or ecukes-message ecukes-verbose)
      (add-to-list 'ecukes-internal-message-log `(message . ,message) t 'eq)
      ad-do-it)))

(defadvice princ (around princ-around activate)
  (let ((message (or (car (ad-get-args 0)) "")))
    (unless ecukes-message
      (add-to-list 'ecukes-message-log message t 'eq))
    (when (or ecukes-message ecukes-verbose)
      (add-to-list 'ecukes-internal-message-log `(princ . ,message) t 'eq)
      ad-do-it)))

(defadvice print (around print-around activate)
  (add-to-list 'ecukes-internal-message-log `(print . ,ad-do-it) t 'eq))

(defun ecukes-quit (&optional exit-code)
  "Quit Emacs with EXIT-CODE and write to file if in graphical mode."
  (or exit-code (setq exit-code 1))
  (let ((outfile (getenv "ECUKES_OUTFILE")))
    (when outfile
      (let ((output
             (-map
              (lambda (log)
                (let ((message (cdr log)))
                  (if (eq (car log) 'print)
                      (prin1-to-string message)
                    message)))
              ecukes-internal-message-log)))
        (f-write-text (s-join "" output) 'utf-8 outfile))))
  (kill-emacs exit-code))

(defun ecukes-fail (format-string &rest objects)
  "Print error message and quit."
  (let ((ecukes-message t))
    (message (apply 'ansi-red (cons format-string objects)))
    (ecukes-quit 1)))

(defun ecukes-on-debug (callback)
  "Call CALLBACK with backtrace from debug."
  (add-to-list 'ecukes-debug-callbacks callback 'append))

(defun ecukes-debug (&rest debugger-args)
  "Ecukes debugger.

This is called when an error occurs. The function creates a
decent backtrace and callbacks all functions in
`ecukes-debug-callbacks' with the backtrace."
  (let ((backtrace
         (with-temp-buffer
           (set-buffer-multibyte t)
           (let ((standard-output (current-buffer))
                 (print-escape-newlines t)
                 (print-level 8)
                 (print-length 50))
             (backtrace))
           (goto-char (point-min))
           (delete-region
            (point)
            (progn
              (search-forward "\n  ecukes-debug(")
              (forward-line 1)
              (point)))
           (buffer-string))))
    (-each ecukes-debug-callbacks
           (lambda (callback)
             (funcall callback backtrace)))))

(provide 'ecukes-core)

;;; ecukes-core.el ends here

