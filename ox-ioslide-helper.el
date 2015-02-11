;;; ox-ioslide-helper.el --- Some small tools to help user write Org document for ox-ioslide  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Kuan, Yen (kuanyui)

;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ˊ・ω・ˋ

;;; Code:
(require 'makey)
(require 'org)
(require 'ox-ioslide)

(setq ioslide-helper-menus
      '(
	(ioslide
	 (description "Org-ioslide tools")
	 (actions
	  ("Which"
	   ("a" "Atttribute (for object in page)"   makey-key-mode-popup-ioslide-attribute)
	   ("p" "Property (for single page)"        makey-key-mode-popup-ioslide-property)
	   ("o" "Option (for the whole slide file)" makey-key-mode-popup-ioslide-option))))
	(ioslide-attribute
	 (description "[Attribute] Apply on object in page")
	 (actions
	  ("List"
	   ("b" "Build List" ioslide/build-list)
	   ("f" "Build + Fade List" ioslide/build-fade-list))
	  ("Image"
	   ("w" "Width" ioslide/image-width))
	  ("Block"
	   ("n" "Speaker Note" ioslide/speaker-note)
	   )))
	(ioslide-property
	 (description "[Property] Apply on a single page.")
	 (actions
	  ("Set Page As..."
	   ("m" "Smaller Font" ioslide/smaller)
	   ("s" "Segue" ioslide/segue)
	   ("f" "Fill Image" ioslide/fill-image)
	   ("t" "Thank-you-slide" ioslide/thank-you-slide))))
	(ioslide-option
	 (description "[Option] Apply on the whole slode file")
	 (actions
	  ("Insert"
	   ("o" "Insert options template" ioslide/insert-options-template)
	   ("m" "Remove MathJax from this slide file to save space" ioslide/remove-mathjax))
	  ))
	)
      )
(makey-initialize-key-groups ioslide-helper-menus)

(defalias 'ioslide:helper 'makey-key-mode-popup-ioslide)
(defalias 'ioslide:attribute 'makey-key-mode-popup-ioslide-attribute)
(defalias 'ioslide:property 'makey-key-mode-popup-ioslide-property)
(defalias 'ioslide:option 'makey-key-mode-popup-ioslide-option)



(setq ioslide-options-template
      "* Slide Options                           :noexport:
# ======= Appear in cover-slide ====================
#+TITLE: Title
#+SUBTITLE: Here Is Subtitle
#+COMPANY: Company Name

# ======= Appear in thank-you-slide ================
#+GOOGLE_PLUS: https://plus.google.com/YOUR_ACCOUNT
#+WWW: http://your.blog.io/
#+GITHUB: http://github.com/YOUR_ACCOUNT
#+TWITTER: USER_NAME

# ======= Appear under each slide ==================
#+FAVICON: images/emacs-icon.png
#+ICON: images/org-icon.png
#+HASHTAG: Hash tag will appear at left-bottom

# ======= Google Analytics =========================
#+ANALYTICS: UA-000000000-0

")


;; ======================================================
;; Attribute
;; ======================================================

(defmacro ioslide--define-attribute-inserter (name string &optional after)
  "Define attribute inserting function conveniently.
The ugly-looking indentation mechanism is for build list."
  `(defun ,(intern (concat "ioslide/" name)) ()
     (interactive)
     (let (indent)
       (back-to-indentation)
       (setq indent (current-column))
       (beginning-of-line)
       (if (not (eolp)) (open-line 1))
       (insert (concat (make-string indent (string-to-char " "))
		       "#+ATTR_HTML: " ,string))
       (unless (eolp) (org-return-indent))
       ,after)))

(ioslide--define-attribute-inserter "build-list" ":class build")
(ioslide--define-attribute-inserter "build-fade-list" ":class build fade")
(ioslide--define-attribute-inserter "image-width"
				    (format ":width %s"
					    (read-from-minibuffer
					     "Width (recommended using px instead of %): ")))
(ioslide--define-attribute-inserter "speaker-note"
				    ":class note\n#+BEGIN_QUOTE\n\n#+END_QUOTE\n"
				    (previous-line 2))


;; ======================================================
;; Property
;; ======================================================
(defun ioslide--put-property (prop-list)
  (org-entry-put nil (nth 0 prop-list) (nth 1 prop-list)))

(defun ioslide/segue ()
  (interactive)
  (mapc #'ioslide--put-property
	'(("SLIDE" "segue dark quote")
	  ("ASIDE" "right bottom")
	  ("ARTICLE" "flexbox vleft auto-fadein"))))

(defun ioslide/fill-image ()
  (interactive)
  (mapc #'ioslide--put-property
	'(("FILL"  "images/sky.jpg") ;[MAYBE] a prompt for file location
	  ("TITLE" "white")
	  ("SLIDE" "white"))))

(defun ioslide/smaller ()
  (interactive)
  (mapc #'ioslide--put-property
	'(("ARTICLE" "smaller"))))

(defun ioslide/thank-you-slide ()
  (interactive)
  (mapc #'ioslide--put-property
	'(("SLIDE"   "thank-you-slide segue")
	  ("ASIDE"   "right")
	  ("ARTICLE" "flexbox vleft auto-fadein"))))


;; ======================================================
;; Option
;; ======================================================

(defun ioslide/insert-options-template ()
  (interactive)
  (goto-char (point-min))
  (insert ioslide-options-template))

;; [MAYBE] maybe detect if title "Slide Options :noexport:" exists
;; then decide if insert a "* Slide Options :noexport:"
(defun ioslide/remove-mathjax ()
  (interactive)
  (if (> (current-column) 0) (beginning-of-line))
  (insert "#+USE_MATHJAX: false\n"))

(provide 'ox-ioslide-helper)
;;; ox-ioslide-helper.el ends here
