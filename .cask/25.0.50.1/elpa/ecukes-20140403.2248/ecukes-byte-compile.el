(eval-when-compile
  (defvar ecukes-verbose))

(require 'dash)

;; Avoid "Loading vc-git..." messages
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; ecukes-core.el advices `princ' and will only print messages if
;; `ecukes-message' and `ecukes-verbose' is true. Byte compiler uses
;; `princ' to display messages, so we set `ecukes-verbose' to true to
;; see the messages.
(setq ecukes-verbose t)

(!cons default-directory load-path)
(!cons load-file-name load-path)

(byte-compile-file (-last-item argv))
