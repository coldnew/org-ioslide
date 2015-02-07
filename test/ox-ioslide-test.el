
(ert-deftest ox-ioslide-test/org-ioslide--copy-resource()
  "Testing copy resource"
  (with-sandbox
   (org-ioslide--copy-resource)
   (should (f-directory? "theme"))
   (should (f-directory? "js"))))

(defmacro ox-ioslide-test/test-export-to-html (file)
  "Testing export in the Org mode file."
  `(let* ((default-directory (file-name-directory ,file))
          (html-file (concat (file-name-sans-extension ,file)
                             ".html"))
          (my-file ,file)
          (visited-p (get-file-buffer my-file))
          to-be-removed)

     (when (file-exists-p html-file) (delete-file html-file))

     ;; Export the file to HTML.
     (save-window-excursion
       (save-match-data
         (find-file my-file)
         (setq to-be-removed (current-buffer))
         (goto-char (point-min))
         (org-ioslide-export-to-html)))
     (unless visited-p (kill-buffer to-be-removed))

     ;; should create a .html file
     (should (file-exists-p html-file))
     ;; should not create a file with "::" appended to it's name
     (should-not (file-exists-p (concat ,file "::")))))

(ert-deftest ox-ioslide-test/org-ioslide-export-to-html/example ()
  "Testing export example file."
  (ox-ioslide-test/test-export-to-html example-file))

(ert-deftest ox-ioslide-test/org-ioslide-export-to-html/sandbox ()
  "Testing copy example file to other dir then export."
  (let ((sandbox-file (f-expand "index.org" root-sandbox-path)))
    (with-sandbox
     ;; Copy example file to sandbox
     (copy-file example-file sandbox-file t)
     ;; Test export file
     (ox-ioslide-test/test-export-to-html sandbox-file))))
