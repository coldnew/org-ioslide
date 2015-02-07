
(ert-deftest test-ois/org-ioslide-export-to-html ()
  "Testing export in the Org mode file."
  (require 'ox-ioslide)
  (let ((html-file (concat (file-name-sans-extension ox-ioslide-test/example-file)
                           ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    ;; Export the file to HTML.
    (org-export-to-file 'ioslide html-file)
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p (concat ox-ioslide-test/example-file "::")))
    (when (file-exists-p html-file) (delete-file html-file))))
