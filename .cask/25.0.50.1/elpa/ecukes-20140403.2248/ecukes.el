;;; ecukes.el --- Cucumber for Emacs

(require 'f)

(defvar ecukes-path (f-dirname (f-this-file)))

(require 'ecukes-run)
(require 'ecukes-stats)
(require 'ecukes-core)
(require 'ecukes-project)
(require 'ecukes-load)
(require 'ecukes-parse)

(require 'ansi-color)

(defvar ecukes-buffer-name "*ecukes*")
(defvar ecukes-include-tags nil)
(defvar ecukes-exclude-tags nil)
(defvar ecukes-cli-reporter "spec")
(defvar ecukes-async-timeout 10)
(defvar ecukes-patterns nil)
(defvar ecukes-anti-patterns nil)
(defvar ecukes-only-failing nil)
(defconst ecukes-failing-scenarios-file ".ecukes-failing-scenarios")

(defun ecukes (&optional ask-for-tags)
  (interactive "P")
  (unless (ecukes-project-path)
    (error "You are not visiting an Ecukes project."))
  (ecukes-load)
  (ecukes-reporter-use ecukes-cli-reporter)
  (when ask-for-tags
    (-each
     (s-split "," (read-string "Run tags: "))
     (lambda (tag)
       (if (s-prefix-p "~" tag)
           (!cons (s-chop-prefix "~@" tag) ecukes-exclude-tags)
         (!cons (s-chop-prefix "@" tag) ecukes-include-tags)))))
  (let ((feature-files
         (if (and (buffer-file-name) (s-matches? "\.feature$" (buffer-file-name)))
             (list (buffer-file-name))
           (f-glob "*.feature" (ecukes-project-features-path)))))
    (let ((ecukes-buffer (get-buffer-create ecukes-buffer-name))
          (buffers (buffer-list))
          (ecukes-internal-message-log)
          (ecukes-stats-steps 0)
          (ecukes-stats-steps-passed 0)
          (ecukes-stats-steps-failed 0)
          (ecukes-stats-steps-skipped 0)
          (ecukes-stats-scenarios 0)
          (ecukes-stats-scenarios-passed 0)
          (ecukes-stats-scenarios-failed 0))
      (ecukes-run feature-files)
      (with-current-buffer ecukes-buffer
        (setq mode-line-process nil)
        (ecukes-mode)
        (read-only-mode -1)
        (erase-buffer)
        (-each
         ecukes-internal-message-log
         (lambda (log)
           (let ((type (car log))
                 (message (cdr log)))
             (when (or (eq type 'message) (eq type 'princ))
               (let ((message-start (point)))
                 (insert (ansi-color-apply message))
                 (when (get-text-property (1- (point)) 'font-lock-face)
                   (when (eq (elt ansi-color-names-vector 1) (cdr (get-text-property (1- (point)) 'font-lock-face)))
                     (add-text-properties message-start (point) '(ecukes-step-error t)))))))))
        (font-lock-mode t)
        (goto-char (point-min))
        (if (eq ecukes-stats-steps-failed 0)
            (setq mode-line-process '(:propertize  " [0 Failures] " face success))
          (setq mode-line-process `(:propertize ,(concat " ["
                                                         (number-to-string ecukes-stats-steps-failed)
                                                         " Failure"
                                                         (when (> ecukes-stats-steps-failed 1) "s")
                                                         "] ") face error)))
        (read-only-mode 1))
      (-each
       (buffer-list)
       (lambda (buffer)
         (unless (-contains? buffers buffer)
           (let ((buffer-modified-p nil))
             (kill-buffer buffer)))))
      (unless (string= (buffer-name) ecukes-buffer-name)
        (display-buffer ecukes-buffer)))))

(defun ecukes-goto-next-step-error (&optional recursive-call-p)
  (interactive)
  (let ((prop-change-pos (save-excursion
                           (let (found)
                             (while (and (not found) (next-single-property-change (point) 'ecukes-step-error))
                               (let ((next-pos (next-single-property-change (point) 'ecukes-step-error)))
                                 (if (get-text-property next-pos 'ecukes-step-error)
                                     (progn
                                       (goto-char next-pos)
                                       (back-to-indentation)
                                       (when (eq (current-column) 4)
                                         (setq found next-pos)))
                                   (goto-char (1+ next-pos)))))
                             found))))
    (when prop-change-pos
      (goto-char prop-change-pos))))

(defun ecukes-goto-previous-step-error (&optional recursive-call-p)
  (interactive)
  (let ((prop-change-pos (save-excursion
                           (let (found)
                             (while (and (not found) (previous-single-property-change (point) 'ecukes-step-error))
                               (let ((previous-pos (previous-single-property-change (point) 'ecukes-step-error)))
                                 (if (get-text-property (1+ previous-pos) 'ecukes-step-error)
                                     (progn
                                       (goto-char previous-pos)
                                       (back-to-indentation)
                                       (when (eq (current-column) 4)
                                         (setq found previous-pos)))
                                   (goto-char (1- previous-pos))
                                   (forward-line -1)
                                   (end-of-line))))
                             found))))
    (when prop-change-pos
      (goto-char prop-change-pos)
      (beginning-of-line))))

;;; minor mode
(defvar ecukes-minor-mode-map nil "Keymap for ecukes-minor-mode.")

(defun ecukes-minor-mode-define-keys ()
  "Defines the key mappings for ecukes-minor-mode."
  (define-key ecukes-minor-mode-map "\C-ce" 'ecukes))

(defun ecukes-minor-mode-define-menu ()
  "Defines the menu for ecukes-minor-mode"
  (define-key ecukes-minor-mode-map [menu-bar ecukes] (cons "Ecukes" (make-sparse-keymap "Ecukes")))
  (define-key (lookup-key ecukes-minor-mode-map [menu-bar ecukes])
    [run-ecukes]
    '("Run Ecukes" . ecukes)))

(unless ecukes-minor-mode-map
  (setq ecukes-minor-mode-map (make-sparse-keymap))
  (ecukes-minor-mode-define-keys)
  (ecukes-minor-mode-define-menu))

(define-minor-mode ecukes-minor-mode
  "Ecukes minor mode"
  nil " ecukes" ecukes-minor-mode-map
)

;;; major mode
(defvar ecukes-mode-map nil "Keymap for ecukes-mode.")

(defun ecukes-mode-define-keys ()
  "Defines the key mappings for ecukes-mode."
  (define-key ecukes-mode-map "g" 'ecukes)
  (define-key ecukes-mode-map "n" 'ecukes-goto-next-step-error)
  (define-key ecukes-mode-map "p" 'ecukes-goto-previous-step-error)
  (define-key ecukes-mode-map "q" 'quit-window)
  (define-key ecukes-mode-map "z" 'kill-this-buffer))

(defun ecukes-mode-define-menu ()
  "Defines the menu for ecukes-mode"
  (define-key ecukes-mode-map [menu-bar ecukes] (cons "Ecukes" (make-sparse-keymap "Ecukes")))
  (define-key (lookup-key ecukes-mode-map [menu-bar ecukes])
    [run-ecukes]
    '("Run Ecukes" . ecukes)))

(unless ecukes-mode-map
  (setq ecukes-mode-map (make-sparse-keymap))
  (ecukes-mode-define-keys)
  (ecukes-mode-define-menu))

(define-derived-mode ecukes-mode nil "Ecukes"
  "A major mode for running ecukes tests."
)

(provide 'ecukes)

;;; ecukes.el ends here
