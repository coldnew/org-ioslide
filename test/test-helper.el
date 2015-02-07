;;; test-helper.el --- Load Ert if not included in Emacs

(require 'f)
(require 'ert)

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-path
  (f-parent root-test-path))

(defvar root-sandbox-path
  (f-expand ".sandbox" root-path))

(defvar root-example-test
  (f-expand "example" root-path))

(defvar example-file
  (f-expand "index.org" root-example-test))

;; This project uses ert-runner, which in turn uses ox-ioslide so to make
;; sure not those functions are tested, this code unbinds all ox-ioslide
;; functions.
;; (unload-feature 'ox-ioslide 'force)

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory root-sandbox-path))
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     ,@body))

(require 'ox-ioslide (f-expand "ox-ioslide" root-path))
