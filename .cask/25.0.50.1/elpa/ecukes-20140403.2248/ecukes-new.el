;;; ecukes-new.el --- Setup up Ecukes for a project

(require 'f)
(require 's)
(require 'dash)
(require 'ansi)

(require 'ecukes-template)

(defvar ecukes-new-features-path
  (f-expand "features" default-directory))

(defvar ecukes-new-project-name
  (f-filename default-directory))

(defun ecukes-new ()
  "Create new Ecukes setup for project."
  (if (ecukes-new-exists-p)
      (error
       (ansi-red "Ecukes already exists for this project"))
    (ecukes-new-create-root)
    (ecukes-new-create-step-definitions)
    (ecukes-new-create-support)
    (ecukes-new-create-feature)))

(defun ecukes-new-create-root ()
  "Create features directory."
  (f-mkdir ecukes-new-features-path)
  (ecukes-new-message 0 "features"))

(defun ecukes-new-create-step-definitions ()
  "Create step-definitions directory and step definition."
  (let ((step-definitions-path (f-expand "step-definitions" ecukes-new-features-path)))
    (f-mkdir step-definitions-path)
    (ecukes-new-message 2 "step-definition")
    (let ((step-definition
           (f-expand (format "%s-steps.el" ecukes-new-project-name) step-definitions-path)))
      (ecukes-template-write step-definition 'step-definition))
    (ecukes-new-message 4 ecukes-new-project-name "-steps.el")))

(defun ecukes-new-create-support ()
  "Create support directory."
  (let ((support (f-expand "support" ecukes-new-features-path)))
    (f-mkdir support)
    (ecukes-new-message 2 "support")
    (let ((env (f-expand "env.el" support)))
      (ecukes-template-write env 'env `(("project-name" . ,ecukes-new-project-name)))))
  (ecukes-new-message 4 "env.el"))

(defun ecukes-new-create-feature ()
  "Create feature file."
  (let ((feature
         (f-expand
          (format "%s.feature" ecukes-new-project-name) ecukes-new-features-path)))
    (ecukes-template-write feature 'feature))
  (ecukes-new-message 2 (format "%s.feature" ecukes-new-project-name)))

(defun ecukes-new-exists-p ()
  "Check if there already exist an Ecukes setup."
  (f-dir? ecukes-new-features-path))

(defun ecukes-new-message (indent &rest objects)
  "Print message about created file."
  (let ((ecukes-verbose t))
    (message
     "create%s%s"
     (s-repeat (1+ indent) " ")
     (ansi-green (apply 's-concat objects)))))


(provide 'ecukes-new)

;;; ecukes-new.el ends here
