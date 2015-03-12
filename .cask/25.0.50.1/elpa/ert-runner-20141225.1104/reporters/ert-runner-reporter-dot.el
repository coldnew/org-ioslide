;;; ert-runner-reporter-dot.el --- One dot per test reporter

;; Copyright (C) 2013 Johan Andersson

;; Author: Jorgen Schäfer
;; Maintainer: Jorgen Schäfer
;; URL: http://github.com/rejeep/ert-runner.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defvar ert-runner-reporter-dot-start-time nil
  "When the last run started.")

(add-hook 'ert-runner-reporter-run-started-functions
          (lambda (stats)
            (setq ert-runner-reporter-dot-start-time (current-time))))

(add-hook 'ert-runner-reporter-run-ended-functions
          (lambda (stats abortedp)
            (let ((unexpected (ert-stats-completed-unexpected stats))
                  (expected-failures (ert--stats-failed-expected stats)))
              (ert-runner-message "\n\n%sRan %s test%s in %.3f seconds%s\n"
                                  (if (not abortedp)
                                      ""
                                    "Aborted: ")
                                  (ert-stats-total stats)
                                  (if (= (ert-stats-total stats) 1)
                                      ""
                                    "s")
                                  (time-to-seconds
                                   (time-subtract (current-time)
                                                  ert-runner-reporter-dot-start-time))
                                  (if (zerop expected-failures)
                                      ""
                                    (format "\n%s expected failures" expected-failures)))
              (unless (zerop unexpected)
                (ert-runner-message "%s unexpected results:\n" unexpected)
                (cl-loop for test across (ert--stats-tests stats)
                         for result = (ert-test-most-recent-result test) do
                         (when (not (ert-test-result-expected-p test result))
                           (ert-runner-message "%9s  %S\n"
                                               (ert-string-for-test-result
                                                result nil)
                                               (ert-test-name test))))))))

(add-hook 'ert-runner-reporter-test-ended-functions
          (lambda (stats test result)
            (if (ert-test-result-expected-p test result)
                (ert-runner-message ".")
              (ert-runner-message "F"))))

(provide 'ert-runner-reporter-dot)
;;; ert-runnert-reporter-dot.el ends here
