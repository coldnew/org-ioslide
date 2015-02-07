;;; ert-loader.el --- Load Ert if not included in Emacs

(require 'f)
(require 'ert)

(defvar ox-ioslide-test/test-path
  (f-dirname (f-this-file)))

(defvar ox-ioslide-test/root-path
  (f-parent ox-ioslide-test/test-path))

(defvar ox-ioslide-test/example-path
  (f-expand "example" ox-ioslide-test/root-path))

(defvar ox-ioslide-test/example-file
  (f-expand "index.org" ox-ioslide-test/example-path))

;; This project uses ert-runner, which in turn uses ox-ioslide so to make
;; sure not those functions are tested, this code unbinds all ox-ioslide
;; functions.
;; (unload-feature 'ox-ioslide 'force)

(load (f-expand "ox-ioslide" ox-ioslide-test/root-path))
