;;; ox-ioslide.el --- Export org-mode to Google I/O HTML5 slide..

;; Copyright (c) 2013 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: html presentation
;; X-URL: http://github.com/coldnew/org-ioslide
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Note: This emacs plugin is under hardly development.
;;
;; It's not suggest to use this plugin since
;; any function may be renamed or modified later.

;; ox-ioslide.el works like org-html5presentation.el and generate
;; Google I/O 2013 slide template.
;;

;;; Installation (not done yet):

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;      M-x package-install org-ioslide
;;
;; In your .emacs
;;
;;      (require 'ox-ioslide)

;;; Code:

;;; Dependencies

(require 'ox-html)
(eval-when-compile (require 'cl))


;;; User Configuration Variables
(defgroup org-export-ioslide nil
  "Options for exporting Org mode files to HTML5 slide."
  :tag "Org Export to Google I/O HTML5 slide"
  :group 'org-export)

(defcustom org-ioslide-resource-url
  "https://io-2012-slides.googlecode.com/git/"
  "Url to get Google I/O js and css resources."
  :group 'org-export-ioslide
  :type 'string)

(defcustom org-ioslide-config-file
  "slide_config.js"
  "Slide config file for Google I/O slide."
  :group 'org-export-ioslide
  :type 'string)

(defcustom org-ioslide-hlevel 1
  "The minimum level of headings that should be grouped into
vertical slides."
  :group 'org-export-ioslide
  :type 'integer)

;;; Define Back-End

(org-export-define-derived-backend 'ioslide 'html
  :menu-entry
  '(?I "Export to Google I/O HTML5 HTML slide."
       ((?I "To file" org-ioslide-export-to-html)))

  :filters-alist '((:filter-parse-tree . org-ioslide-generate-config-file))

  :options-alist
  '(
    ;; Configs that will be generated in slide_config.js

    ;; subtitle
    (:subtitle          "SUBTITLE"          nil   nil   t)
    ;; useBuilds, Default: true, False will turn off slide animation builds.
    (:use-builds        "USE_BUILDS"        nil "true"  t)
    ;; usePrettify, Default: true
    (:use-prettify      "USE_PRETTIFY"      nil "true"  t)
    ;; enableSlideAreas, Default: true. False turns off the click
    ;; areas on either slide of the slides.
    (:enable-slideareas "ENABLE_SLIDEAREAS" nil "true"  t)
    ;; enableTouch, Default: true. If touch support should enabled.
    ;; Note: the dvice must support touch.
    (:enable-touch      "ENABLE_TOUCH"      nil "true"  t)
    ;; favIcon
    (:fav-icon          "FAVICON"           nil    ""   t)
    ;; TODO: fonts
    ;; Author information
    (:company           "COMPANY"           nil   nil   t)
    (:google-plus       "GOOGLE_PLUS"       nil   nil   t)
    (:twitter           "TWITTER"           nil   nil   t)
    (:www               "WWW"               nil   nil   t)
    (:github            "GITHUB"            nil   nil   t)

    ;; Other configs

    ;; Google analytics: 'UA-XXXXXXXX-1
    (:analytics         "ANALYTICS"         nil   nil   t)
    (:logo              "LOGO"              nil    ""   t)
    (:icon              "ICON"              nil    ""   t)
    (:hlevel            "HLEVEL"            nil   nil   t)
    )

  :translate-alist
  '((headline     . org-ioslide-headline)
    (section      . org-ioslide-section)
    (template     . org-ioslide-template)
    (center-block . org-ioslide-center-block)
    (src-block    . org-ioslide-src-block)
    (export-block . org-ioslide-export-block)
    )

  :export-block '("NOTES")
  )


;;; Internal Functions
(defun org-ioslide-if-format (fmt val)
  (let ((str (if (listp val)
                 (or (car-safe val) "")
               val)))
    (if val (format fmt str))))

(defun org-ioslide-plist-get-string (info key)
  (let ((r (plist-get info key)))
    (if (stringp r) r (or (car r) ""))))

(defun org-ioslide-close-element (element element-extra body)
  (format "<%s %s>\n%s\n</%s>" element element-extra body element))

(defun org-ioslide--download-resource ()
  "Download needed rsouce from org-ioslide-resource-url."
  (let ((url org-ioslide-resource-url)
        (dir-list  '("js" "js/polyfills" "js/prettify" "theme" "theme/css" "theme/scss"))
        (file-list '(;; Files in js dir
                     ("js/" .
                      ("hammer.js" "modernizr.custom.45394.js" "order.js"
                       "require-1.0.8.min.js" "slide-controller.js" "slide-deck.js" "slides.js"))
                     ;; Files in js/polyfills dir
                     ("js/polyfills/" .
                      ("classList.min.js" "dataset.min.js" "history.min.js"))
                     ;; Files in js/prettify dir
                     ("js/prettify/" .
                      ("lang-apollo.js" "lang-css.js" "lang-hs.js" "lang-lua.js"
                       "lang-n.js" "lang-scala.js" "lang-tex.js""lang-vhdl.js"
                       "lang-xq.js""prettify.css" "lang-clj.js" "lang-go.js"
                       "lang-lisp.js" "lang-ml.js" "lang-proto.js" "lang-sql.js"
                       "lang-vb.js" "lang-wiki.js" "lang-yaml.js" "prettify.js"))
                     ;; Files in theme/css dir
                     ("theme/css/"  .
                      ("default.css" "io2013.css" "phone.css"))
                     ;; Files in theme/scss dir
                     ("theme/scss/" .
                      ("_base.scss" "default.scss" "io2013.scss" "phone.scss" "_variable.scss"))
                     )))

    ;; Create parent directory
    (dolist (d dir-list) (make-directory d t))

    ;; Download files
    (dolist (fl file-list)
      (dolist (f (cdr fl))
        (let ((target (concat (car fl) f)))
          (url-copy-file (concat url target) target t))))))

(defun org-ioslide-generate-config-file (text back-end info)
  (let ((file-name org-ioslide-config-file))
    (save-excursion
      (with-temp-file file-name
        (insert
         (concat
          "var SLIDE_CONFIG = {
   // Slide Settings
   settings: {
"
          ;; title
          (format
           "     title: '%s', \n" (org-ioslide-plist-get-string info :title))
          ;; subtitle
          (format
           "     subtitle: '%s', \n" (org-ioslide-plist-get-string info :subtitle))
          ;; useBuilds
          (format
           "     useBuilds: %s, " (org-ioslide-plist-get-string info :use-builds))
          "// Default: true. False will turn off slide animation builds. \n"
          ;; usePrettify
          (format
           "     usePrettify: %s, " (org-ioslide-plist-get-string info :use-prettify))
          "// Default: true \n"
          ;; enableSlideAreas
          (format
           "     enableSlideAreas: %s, " (org-ioslide-plist-get-string info :enable-slideareas))
          "// Default: true. False turns off the click areas on either slide of the slides.\n"
          ;; enableTouch
          (format
           "     enableTouch: %s, " (org-ioslide-plist-get-string info :enable-touch))
          "// Default: true. If touch support should enabled. Note: the device must support touch.\n"
          ;; favIcon
          (format
           "     favIcon: '%s', \n" (org-ioslide-plist-get-string info :fav-icon))
          ;; TODO: fonts
          "     fonts: [
       'Open Sans:regular,semibold,italic,italicsemibold',
       'Source Code Pro'
     ],\n"
          "   }, \n \n"
          ;; Author information
          "   // Author information
   presenters: [{\n"
          ;; name
          (format
           "     name: '%s', \n" (org-ioslide-plist-get-string info :author))
          ;; company
          (format
           "     company: '%s', \n" (org-ioslide-plist-get-string info :company))
          ;; google plus
          (format
           "     gplus: '%s', \n" (org-ioslide-plist-get-string info :google-plus))
          ;; twitter
          (format
           "     twitter: '%s', \n" (org-ioslide-plist-get-string info :twitter))
          ;; www
          (format
           "     www: '%s', \n" (org-ioslide-plist-get-string info :www))
          ;; github
          (format
           "     github: '%s', \n" (org-ioslide-plist-get-string info :github))

          "   }]
};"
          ))))))


;;; Transcode Functions

(defun org-ioslide-get-hlevel (info)
  "Get HLevel value safely.
If option \"HTML5SLIDE_HLEVEL\" is set, retrieve integer value from it,
else get value from custom variable `org-ioslide-hlevel'."
  (let ((hlevel-str (plist-get info :hlevel)))
    (if hlevel-str (string-to-number hlevel-str)
      org-ioslide-hlevel)))

;;;; Center Block
(defun org-ioslide-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format
   "<article class=\"flexbox vcenter\">\n%s</article>" contents))


;;;; Note Block
(defun org-ioslide-export-block (export-block contents info)
  "Transocde a EXPORT-BLOCK element from Org to ioslide.
CONTENTS is nil. NFO is a plist holding contextual information."
  (if (string= (org-element-property :type export-block) "NOTES")
      (concat
       "<aside class=\"note\">\n <section>\n"
       (org-element-property :value export-block)
       "</section>\n</aside>\n")
    (org-html-export-block export-block contents info)))

;;;; Src Block

(defun org-ioslide--encode-src-text (code)
  "Encode source code to plane text, if source contains <b> or </b>,
make it translate to html tags."
  (replace-regexp-in-string
   "&lt;b&gt;"  "<b>"
   (replace-regexp-in-string
    "&lt;/b&gt;" "</b>"
    (org-html-encode-plain-text code))))

(defun org-ioslide--src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (caption (org-export-get-caption src-block))
          (code
           ;; If not use-prettify, use org-html's method
           ;; to renderd src block
           (if (string= "true" (plist-get info :use-prettify))
               (org-ioslide--encode-src-text (car (org-export-unravel-code src-block)))
             (org-html-format-code src-block info)))

          (label (let ((lbl (org-element-property :name src-block)))
                   (if (not lbl) ""
                     (format " id=\"%s\""
                             (org-export-solidify-link-text lbl))))))
      (if (not lang)
          ;; Use org-html-src-block to genterate HTML code.
          (org-html-src-block src-block contents info)
        ;; Use prettyprint to generate source block
        (format
         "<div class=\"org-src-container\">\n%s%s\n</div>"
         (if (not caption) ""
           (format "<label class=\"org-src-name\">%s</label>"
                   (org-export-data caption info)))
         (format "\n<pre class=\"prettyprint\" data-lang=\"%s\"%s>\n%s</pre>\n" lang label code))

        ))))

(defun org-ioslide-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
If #+USE_PRETTIFY is `true' use org-ioslide--src-block to render the code block.
Else use org-html-src-block to convert source block to html."
  (org-ioslide--src-block src-block contents info))

;;;; Google Analytics
(defun org-ioslide-google-analytics (info)
  (let ((user-id (or (plist-get info :analytics) "UA-XXXXXXXX-1")))
    (format
     (concat "<script>\n"
             "var _gaq = _gaq || []; \n"
             "_gaq.push(['_setAccount', '%s']);\n"
             "_gaq.push(['_trackPageview']); \n"
             "(function() {
  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
})();
"
             "</script>\n"
             ) user-id)))


;;;; title
(defun org-ioslide--handle-title-group (headline info &optional class title-class no-title)
  (let ((no-title-p (or no-title
                        (if (string= "false" (org-element-property :TITLE headline)) t nil))))
    (if no-title-p ""
      (format
       "\n<hgroup class=\"%s\">
       <h2 class=\"%s\">%s</h2>
       <h3>%s</h3>
       </hgroup>
"
       (or class "")
       ;; headline text.
       (or title-class "")
       (org-html-format-headline--wrap headline info)
       ;; subtitle
       (or (org-element-property :SUBTITLE headline) "")))))

;;;; Segue slide
(defun org-ioslide--handle-slide-id (headline info)
  "Generate <slide> class with id."
  (format "<slide id=\"%s\" "
          (or (org-element-property :CUSTOM_ID headline)
              (concat "sec-" (mapconcat 'number-to-string
                                        (org-export-get-headline-number headline info)
                                        "-")))))

(defun org-ioslide--handle-slide-class (headline contents info)
  "Special handler for segue slide class."
  ;; Check if :SLIDE: property is not nil
  (if (org-element-property :SLIDE headline)
      (let* ((class (org-element-property :SLIDE headline))
             (segue-p (string-match "segue" class))
             (quote-p (string-match "quote" class))
             (thank-p (string-match "thank-you-slide" class)))
        (concat
         ;; Stop previous slide.
         (if (or (/= level 1)
                 (not (org-export-first-sibling-p headline info)))
             "</article></slide>\n")
         ;; slide class
         (org-ioslide--handle-slide-id headline info)
         ;; segue slide
         (format
          "class=\"%s nobackground\" style=\"background-image: url(%s)\">
%s
%s
%s
    </slide>
  "
          ;; class for slide
          (or (org-element-property :SLIDE headline) "")
          ;; background
          (or (org-element-property :BACKGROUND headline) "")
          ;; icon
          (if segue-p
              (format
               "<aside class=\"gdbar %s\"><img src=\"%s\"></aside>"
               (or (org-element-property :ASIDE headline) "")
               ;; get ICON from property, if not exist get ICON from info
               (or (org-element-property :ICON headline)
                   (plist-get info :icon) "")
               ) "")

          ;; handle title
          (org-ioslide--handle-title-group
           ;; hadline
           headline
           ;; info
           info
           ;; if not segue or is quote, disable fadin.
           (if (and segue-p (not quote-p)) "auto-fadein " " ")
           ;; headline text color (select white if has background)
           (if (org-element-property :BACKGROUND headline) "white" "")
           ;;  if this is quote, disable title
           quote-p)

          ;; contents
          (format "<article %s> %s </article>"
                  ;; if define article class or use quote, add class
                  ;; here
                  (format "class=\" %s\"" (concat
                                           (or (org-element-property :ARTICLE headline) "")
                                           (if (or quote-p
                                                   thank-p)
                                               "flexbox vleft auto-fadein" "")))

                  contents)
          )))))

;;;; Other

(defun org-ioslide-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Google I/O slides.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  ;; First call org-html-headline to get the formatted HTML contents.
  ;; Then add enclosing <article> tags to mark slides.
  (setq contents (or contents ""))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
         (level (org-export-get-relative-level headline info))
         (text (org-export-data (org-element-property :title headline) info))
         (todo (and (plist-get info :with-todo-keywords)
                    (let ((todo (org-element-property :todo-keyword headline)))
                      (and todo (org-export-data todo info)))))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info)))
         (priority (and (plist-get info :with-priority)
                        (org-element-property :priority headline)))
         ;; Create the headline text.
         (full-text (org-html-format-headline--wrap headline info)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info)
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'ordered 'unordered))
             (itemized-body (org-html-format-list-item
                             contents type nil info nil full-text)))
        (concat
         (and (org-export-first-sibling-p headline info)
              (org-html-begin-plain-list type))
         itemized-body
         (and (org-export-last-sibling-p headline info)
              (org-html-end-plain-list type)))))

     ;;     ((org-element-property :SLIDE headline) (org-ioslide--handle-slide-class headline contents info))

     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((section-number (mapconcat 'number-to-string
                                        (org-export-get-headline-number
                                         headline info) "-"))
             (ids (remove 'nil
                          (list (org-element-property :CUSTOM_ID headline)
                                (concat "sec-" section-number)
                                (org-element-property :ID headline))))
             (level1 (+ level (1- org-html-toplevel-hlevel)))
             (hlevel (org-ioslide-get-hlevel info))
             (first-content (car (org-element-contents headline))))

	(org-ioslide-close-element
	 (org-ioslide--container headline info)
	 (format "id=\"%s\" %s"
		 (or (org-element-property :CUSTOM_ID headline)
		     (concat "sec-" section-number))
		 ;; container class
		 (org-ioslide--container-class headline info))
	 ;; body
	 (format "%s%s%s"
		 ;; aside
		 ""
		 ;; title
		 (org-ioslide--title headline info)
		 ;; article
		 (org-ioslide--article headline contents info)))

	)))))
        ;; (format "<%s id=\"%s\" %s>\n %s%s%s</%s>\n"
        ;;         (org-ioslide--container headline info)
        ;;         (format "%s"
        ;;                 (or (org-element-property :CUSTOM_ID headline)
        ;;                     (concat "sec-" section-number)))
	;; 	;; container class
        ;;         (org-ioslide--container-class headline info)
	;; 	;; aside
	;; 	""
	;; 	;; title
        ;;         (org-ioslide--title headline info)
	;; 	;; article
	;; 	(org-ioslide--article headline contents info)

	;; 	(org-ioslide--container headline info)
        ;;         ))))))
;; (concat
;;  ;; ;; Stop previous slide.
;;  ;; (if (or (/= level 1)
;;  ;;         (not (org-export-first-sibling-p headline info)))
;;  ;;     "</article></slide>\n")

;;  ;; Add an extra "<article>" to group following slides
;;  ;; into vertical ones.
;;  ;;         (if (eq level hlevel) "<slide>\n")
;;  ;; Start a new slide.
;;  (org-ioslide--handle-slide-id headline info) " >\n"

;;  ;; Handle Slide title
;;  (org-ioslide--handle-title-group headline info)

;;  ;; Slide article class
;;  (format "<article class=\"%s\">\n" (or (org-element-property :ARTICLE headline) ""))

;;  ;; When there is no section, pretend there is an empty
;;  ;; one to get the correct <div class="outline- ...>
;;  ;; which is needed by `org-info.js'.
;;  (if (not (eq (org-element-type first-content) 'section))
;;      (concat (org-html-section first-content "" info)
;;              contents)
;;    contents)

;;  ;; Add an extra "</article>" to stop vertical slide grouping.
;;  (if (= level hlevel) "</article>\n")

;;  ;; Stop all slides when meets last head 1.
;;  (if (and (= level 1)
;;           (org-export-last-sibling-p headline info))
;;      "</slide>")))))))

(defun org-ioslide--container (headline info)
  "Return the top container of ioslide."
  "slide")

(defun org-ioslide--container-class (headline info)
  "Special handler for segue slide class."
  (format
   "class=\"%s nobackground\" style=\"background-image: url(%s)\""
   ;; class for slide
   (or (org-element-property :SLIDE headline) "")
   ;; background
   (or (org-element-property :BACKGROUND headline) "")))

(defun org-ioslide--title (headline info &optional class title-class no-title)
    (if (string= "false" (org-element-property :TITLE headline)) ""
      (format
       "\n<hgroup class=\"%s\">
       <h2 class=\"%s\">%s</h2>
       <h3>%s</h3>
       </hgroup>
"
       (or class "")
       ;; headline text.
       (or title-class "")
       (org-html-format-headline--wrap headline info)
       ;; subtitle
       (or (org-element-property :SUBTITLE headline) ""))))

(defun org-ioslide-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section. INFO is a plist
holding contextual information."
  ;; Just return the contents. No "<div>" tags.
  contents)

(defun org-ioslide--article (headline contents info)
  ;; When there is no section, pretend there is an empty
  ;; one to get the correct <div class="outline- ...>
  ;; which is needed by `org-info.js'.
  (let ((container "article"))
  (format "<%s>\n%s</%s>\n"
	  container
	   (if (not (eq (org-element-type first-content) 'article))
	       (concat (org-html-section first-content "" info)
		       contents)
	     contents)
	   container)))

;;; Template

(defun org-ioslide--build-logo-slide (info)
  (let ((logo-file (plist-get info :logo)))
    (if (< 0 (string-width logo-file))
        (format
         "<slide class=\"logoslide nobackground\">
               <article class=\"flexbox vcenter\">
                 <span><img src=\"%s\"></span>
               </article>
             </slide>\n"
         logo-file))))

(defun org-ioslide--build-title-slide (info)
  (format
   "<slide class=\"title-slide segue nobackground\">
       <aside class=\"gdbar\"><img src=\"%s\"></aside>
       <!-- The content of this hgroup is replaced programmatically through the slide_config.json. -->
       <hgroup class=\"auto-fadein\">
         <h1 data-config-title><!-- populated from slide_config.json --></h1>
         <h2 data-config-subtitle><!-- populated from slide_config.json --></h2>
         <p data-config-presenter><!-- populated from slide_config.json --></p>
       </hgroup>
    </slide>
  "
   (plist-get info :icon)))

(defun org-ioslide--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
         (lambda (str)
           (replace-regexp-in-string
            "\"" "&quot;" (org-html-encode-plain-text str))))
        (author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       (and auth
                            ;; Return raw Org syntax, skipping non
                            ;; exportable objects.
                            (org-element-interpret-data
                             (org-element-map auth
                                 (cons 'plain-text org-element-all-objects)
                               'identity info))))))
        (description (plist-get info :description))
        (keywords (plist-get info :keywords))
        (charset (or (and org-html-coding-system
                          (fboundp 'coding-system-get)
                          (coding-system-get org-html-coding-system
                                             'mime-charset))
                     "iso-8859-1")))
    (concat
     "<title></title>\n"
     (when (plist-get info :time-stamp-file)
       (format-time-string
        (concat "<!-- " org-html-metadata-timestamp-format " -->\n")))

     (format
      (org-html-close-tag "meta" " charset=\"%s\"" info) charset)
     "\n"
     (format
      (org-html-close-tag "meta" " htto-equiv=\"%s\" content=\"%s\"" info) "X-UA-Compatible" "chrome=1")
     "\n"
     (org-html-close-tag "meta" " name=\"generator\" content=\"Org-mode with org-ioslide\"" info)
     "\n"
     (if (org-string-nw-p author)
         (org-html-close-tag "meta"
                             (format " name=\"author\" content=\"%s\""
                                     (funcall protect-string author))
                             info))
     "\n"
     (if (org-string-nw-p description)
         (org-html-close-tag "meta"
                             (format " name=\"description\" content=\"%s\"\n"
                                     (funcall protect-string description))
                             info))
     "\n"
     (if (org-string-nw-p keywords)
         (org-html-close-tag "meta"
                             (format " name=\"keywords\" content=\"%s\""
                                     (funcall protect-string keywords))
                             info))
     "\n")))

(defun org-ioslide--build-stylesheets (info)
  "Return the HTML contents for declaring ioslide stylesheets."
  (concat
   (org-html-close-tag "meta"
                       "name=\"apple-mobile-web-app-capable\" content=\"yes\""
                       info)
   "\n"
   (org-html-close-tag "link"
                       "rel=\"stylesheet\" media=\"all\" href=\"theme/css/default.css\""
                       info)
   "\n"
   (org-html-close-tag "link"
                       "rel=\"stylesheet\" media=\"only screen and (max-device-width: 480px)\" href=\"theme/css/phone.css\""
                       info)
   "\n"
   "<script data-main=\"js/slides\" src=\"js/require-1.0.8.min.js\"></script>"
   "\n"))

(defun org-ioslide-template (contents info)
  "Return complete document string after HTML conversion.
contents is the transoded contents string.
info is a plist holding eport options."
  (concat
   "<!DOCTYPE html>
<html>
<head>
"
   ;; Create meta info
   (org-ioslide--build-meta-info info)

   ;; Other meta info.... well, I don't know what they do.....
   "<!--<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, minimum-scale=1.0\">-->\n"
   "<!--<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">-->\n"
   "<!--This one seems to work all the time, but really small on ipad-->\n"
   "<!--<meta name=\"viewport\" content=\"initial-scale=0.4\">-->\n"

   ;; Import stylesheet from ioslide
   (org-ioslide--build-stylesheets info)
   "</head>
<body style=\"opacity: 0\">
"

   "<slides class=\"layout-widescreen\">\n"

   ;; Logo Slide
   (org-ioslide--build-logo-slide info)

   ;; Title Slide
   (org-ioslide--build-title-slide info)

   ;; Slide contents
   contents

   "<slide class=\"backdrop\"></slide>\n"
   "</slides> \n"

   ;; Google Analytics
   (org-ioslide-google-analytics info)

   "<!--[if IE]>
  <script src=\"http://ajax.googleapis.com/ajax/libs/chrome-frame/1/CFInstall.min.js\"></script>
  <script>CFInstall.check({mode: 'overlay'});</script>
<![endif]-->
"

   "</body> \n
</html>\n"
   ))


;;; End-user functions

;;;###autoload
(defun org-ioslide-download-resource ()
  "Download extra resource like css or js file to use this slide offline."
  (interactive)
  (message "Start download ioslide resource!")
  (org-ioslide--download-resource)
  (message "Download ioslide resource Finish!"))

;;;###autoload
(defun org-ioslide-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

Export is done in a buffer named \"*Org HTML5 Slide Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (let ((outbuf (org-export-to-buffer
                 'ioslide "*Org Google I/O HTML5 Slide Export*"
                 subtreep visible-only body-only ext-plist))
        (org-export-coding-system org-html-coding-system))
    ;; Set major mode.
    (with-current-buffer outbuf (set-auto-mode t))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))

;;;###autoload
(defun org-ioslide-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Google ioslide HTML5 slide HTML file."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file
     'ioslide file subtreep visible-only body-only ext-plist)))

;; TODO:
;; Fix center not center
;; hoghtlight in table
;; table color
;; table option B
;; maybe remove slide_config.js ?

(provide 'ox-ioslide)
;;; ox-ioslide.el ends here
