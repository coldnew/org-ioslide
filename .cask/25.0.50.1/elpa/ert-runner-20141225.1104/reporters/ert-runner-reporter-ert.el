;;; ert-runner-reporter-ert.el --- ERT-style reporter

;; Most of the code here is taken directly from ert.el's
;; `ert-run-tests-batch' function.

(require 'ert nil 'no-error)

(add-hook 'ert-runner-reporter-run-started-functions
          (lambda (stats)
            (ert-runner-message "Running %s tests (%s)\n\n"
                                (length (ert--stats-tests stats))
                                (ert--format-time-iso8601
                                 (ert--stats-start-time stats)))))

(add-hook 'ert-runner-reporter-run-ended-functions
          (lambda (stats abortedp)
            (let ((unexpected (ert-stats-completed-unexpected stats))
                  (expected-failures (ert--stats-failed-expected stats)))
              (ert-runner-message "\n%sRan %s tests, %s results as expected%s (%s)%s\n\n"
                                  (if (not abortedp)
                                      ""
                                    "Aborted: ")
                                  (ert-stats-total stats)
                                  (ert-stats-completed-expected stats)
                                  (if (zerop unexpected)
                                      ""
                                    (format ", %s unexpected" unexpected))
                                  (ert--format-time-iso8601
                                   (ert--stats-end-time stats))
                                  (if (zerop expected-failures)
                                      ""
                                    (format "\n%s expected failures"
                                            expected-failures)))
              (unless (zerop unexpected)
                (ert-runner-message "%s unexpected results:\n" unexpected)
                (cl-loop for test across (ert--stats-tests stats)
                         for result = (ert-test-most-recent-result test) do
                         (when (not (ert-test-result-expected-p test result))
                           (ert-runner-message "%9s  %S\n"
                                               (ert-string-for-test-result
                                                result nil)
                                               (ert-test-name test))))
                (ert-runner-message "\n")))))

(add-hook 'ert-runner-reporter-test-ended-functions
          (lambda (stats test result)
            (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                   (format-string (concat "%9s  %"
                                          (prin1-to-string (length max))
                                          "s/" max "  %S\n")))
              (ert-runner-message format-string
                                  (ert-string-for-test-result
                                   result
                                   (ert-test-result-expected-p
                                    test result))
                                  (1+ (ert--stats-test-pos stats test))
                                  (ert-test-name test)))))

(provide 'ert-runner-reporter-ert)
