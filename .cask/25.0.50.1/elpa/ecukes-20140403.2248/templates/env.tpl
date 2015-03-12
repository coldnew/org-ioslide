(require 'f)

(defvar {{project-name}}-support-path
  (f-dirname load-file-name))

(defvar {{project-name}}-features-path
  (f-parent {{project-name}}-support-path))

(defvar {{project-name}}-root-path
  (f-parent {{project-name}}-features-path))

(add-to-list 'load-path {{project-name}}-root-path)

(require '{{project-name}})
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
