;;; ecukes-cli.el --- Entry point when running Ecukes from terminal -*- lexical-binding: t; -*-

(require 'f)
(require 's)
(require 'dash)
(require 'commander)
(require 'debug)

(defvar ecukes-path (f-dirname (f-this-file)))

(add-to-list 'load-path ecukes-path)

(require 'ecukes-core)
(require 'ecukes-load)
(require 'ecukes-new)
(require 'ecukes-project)
(require 'ecukes-run)
(require 'ecukes-stats)
(require 'ecukes-reporter)



(eval-when-compile
  (defvar ecukes-error-log-file))

(setq debug-on-error t)
(setq debug-on-signal t)
(setq debugger 'ecukes-debug)

(defvar ecukes-include-tags nil
  "Scenario tags to include.")

(defvar ecukes-exclude-tags nil
  "Scenario tags to exclude.")

(defvar ecukes-cli-reporter "dot"
  "Default reporter.")

(defvar ecukes-cli-with-doc nil
  "If the list-steps command show doc or not.")

(defvar ecukes-cli-with-file nil
  "If the list-steps command show definition file or not.")

(defvar ecukes-async-timeout 10
  "Timeout for async step definitions.")

(defvar ecukes-patterns nil
  "Run scenarios matching any of the patterns.")

(defvar ecukes-anti-patterns nil
  "Do not run scenarios matching any of the patterns.")

(defvar ecukes-only-failing nil
  "Run only failing scenarios if true.")

(defconst ecukes-failing-scenarios-file ".ecukes-failing-scenarios"
  "File to save list of failing scenarios in.")

(defconst ecukes-error-log "ecukes.err"
  "Default file to log Ecukes error output to.")


;;;; Commands

(defun ecukes-cli/new ()
  "Create new Ecukes files for project."
  (ecukes-new))

(defun ecukes-cli/list-steps ()
  "Print all available steps defined for this project."
  (ecukes-load-step-definitions)
  (ecukes-reporter-print-steps ecukes-cli-with-doc ecukes-cli-with-file))

(defun ecukes-cli/list-reporters ()
  "Show list of available reporters."
  (let ((ecukes-message t))
    (princ " ")
    (-each
     ecukes-reporters
     (lambda (reporter)
       (let ((name (car reporter))
             (description (cdr reporter)))
         (message "  %s - %s" name description))))
    (princ "\n")))


;;;; Options

(defun ecukes-cli/with-doc ()
  "Include docstring when printing steps with 'list-steps'."
  (setq ecukes-cli-with-doc t))

(defun ecukes-cli/with-file ()
  "Include file name when printing steps with 'list-steps'."
  (setq ecukes-cli-with-file t))

(defun ecukes-cli/verbose ()
  "Show output comming from the actual package."
  (setq ecukes-verbose t))

(defun ecukes-cli/quiet ()
  "Do not show output comming from the actual package."
  (setq ecukes-verbose nil))

(defun ecukes-cli/tags (tag-string)
  "Only execute the scenarios with tags matching TAG-STRING.

Examples: --tags @dev, --tags @dev,~@local

A tag starting with ~ excluded from the scenarios."
  (-each
   (s-split "," tag-string)
   (lambda (tag)
     (if (s-prefix-p "~" tag)
         (!cons (s-chop-prefix "~@" tag) ecukes-exclude-tags)
       (!cons (s-chop-prefix "@" tag) ecukes-include-tags)))))

(defun ecukes-cli/run (&rest args)
  (ecukes-load)
  (ecukes-reporter-use ecukes-cli-reporter)
  (let ((feature-files))
    (-each
     args
     (lambda (arg)
       (let ((path (f-expand arg (ecukes-project-path))))
         (if (f-dir? path)
             (-each
              (f-files
               path
               (lambda (file)
                 (s-matches? "\.feature$" file))
               'recursive)
              (lambda (feature-file)
                (!cons feature-file feature-files)))
           (!cons path feature-files)))))
    (ecukes-run feature-files)))

(defun ecukes-cli/help ()
  "Display usage information."
  (let ((ecukes-message t))
    (commander-print-usage))
  (ecukes-quit 0))

(defun ecukes-cli/reporter (reporter)
  "Select reporter (default: dot)."
  (setq ecukes-cli-reporter reporter))

(defun ecukes-cli/timeout (timeout)
  "How long to wait for async steps before quitting."
  (setq ecukes-async-timeout (string-to-number timeout)))

(defun ecukes-cli/patterns (&rest patterns)
  "Run scenarios matching a pattern."
  (setq ecukes-patterns patterns))

(defun ecukes-cli/anti-patterns (&rest patterns)
  "Do not run scenarios matching a pattern."
  (setq ecukes-anti-patterns patterns))

(defun ecukes-cli/only-failing ()
  "Run only failing scenarios."
  (setq ecukes-only-failing t))

(defun ecukes-cli/debug ()
  "Run in debug mode (show all output and stacktraces)."
  (ecukes-on-debug 'princ))

(defun ecukes-cli/error-log (&optional file)
  "Log error backtrace to file."
  (ecukes-on-debug
   (lambda (backtrace)
     (f-write-text backtrace 'utf-8 (or file ecukes-error-log)))))

(defun ecukes-cli/script ()
  "Run Ecukes as a script/batch job (default).")

(defun ecukes-cli/win ()
  "Run Ecukes with full GUI window.")

(defun ecukes-cli/no-win ()
  "Run Ecukes without GUI window.")


;;;; Commander schedule

(setq commander-args (-reject 's-blank? (s-split " " (getenv "ECUKES_ARGS"))))

(commander
 (name "ecukes")
 (description "Cucumber for Emacs")
 (config ".ecukes")

 (default ecukes-cli/run "features")

 (command "new" ecukes-cli/new)
 (command "list-steps" ecukes-cli/list-steps)
 (command "list-reporters" ecukes-cli/list-reporters)

 (option "--with-doc" ecukes-cli/with-doc)
 (option "--with-file" ecukes-cli/with-file)

 (option "--verbose" ecukes-cli/verbose)
 (option "--quiet"  ecukes-cli/quiet)

 (option "-h, --help" ecukes-cli/help)
 (option "--debug" ecukes-cli/debug)
 (option "--tags <tag-string>" ecukes-cli/tags)

 (option "--script" ecukes-cli/script)
 (option "--no-win" ecukes-cli/no-win)
 (option "--win" ecukes-cli/win)

 (option "--reporter <reporter>, -r <reporter>" ecukes-cli/reporter)

 (option "-t <seconds>, --timeout <seconds>" ecukes-cli/timeout)

 (option "-p <*>, --patterns <*>" ecukes-cli/patterns)
 (option "-a <*>, --anti-patterns <*>" ecukes-cli/anti-patterns)

 (option "-f, --only-failing" ecukes-cli/only-failing)
 (option "-l [file], --error-log [file]" ecukes-cli/error-log))



(ecukes-quit
 (if (> ecukes-stats-steps-failed 0) 1 0))

;;; ecukes-cli.el ends here
