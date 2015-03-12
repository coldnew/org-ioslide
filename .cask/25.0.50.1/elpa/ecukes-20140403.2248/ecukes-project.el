;;; ecukes-project.el --- Project helpers

(require 'f)

(defun ecukes-project-path (&optional dir)
  "Path to project."
  (or dir (setq dir default-directory))
  (if (f-dir? (f-expand "features" dir))
      dir
    (let ((parent (f-parent dir)))
      (unless (f-root? parent)
        (ecukes-project-path parent)))))

(defun ecukes-project-name ()
  "Name of the project."
  (f-filename (ecukes-project-path)))

(defun ecukes-project-features-path ()
  "Path to project features dir."
  (f-expand "features" (ecukes-project-path)))

(defun ecukes-project-support-path ()
  "Path to project features dir."
  (f-expand "support" (ecukes-project-features-path)))

(defun ecukes-project-step-definitions-path ()
  "Path to project step definitions dir."
  (f-expand "step-definitions" (ecukes-project-features-path)))

(provide 'ecukes-project)

;;; ecukes-project.el ends here
