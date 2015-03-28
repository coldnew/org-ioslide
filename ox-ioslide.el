;;; ox-ioslide.el --- Export org-mode to Google I/O HTML5 slide.

;; Copyright (c) 2013-2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;; Copyright (c) 2015 Kuan, Yen. (kuanyui) <azazabc123@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: html presentation
;; X-URL: http://github.com/coldnew/org-ioslide
;; Version: 0.2
;; Package-Requires: ((emacs "24.1") (org "8.0") (cl-lib "0.5") (f "0.17.2") (makey "0.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; About ox-ioslide.el

;; ox-ioslide.el is an org-mode exporter to export org file to Google I/O 2013
;; style slides.

;; DEMO site: http://coldnew.github.io/org-ioslide

;;; Installation:

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;      M-x package-install ox-ioslide
;;
;; In your .emacs
;;
;;      (require 'ox-ioslide)
;;
;; For more information, please see `README.org'
;;

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ox-html)
(require 'f)

(defvar org-ioslide-path
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Get the absolute path of this file. Don't change this manually.")

(defgroup org-export-ioslide nil
  "Options for exporting Org mode files to HTML5 slide."
  :tag "Org Export to Google I/O HTML5 slide"
  :group 'org-export)


;;; User Configuration Variables

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

(defvar org-ioslide--current-footnote-list nil)

;;; Define Back-End

(org-export-define-derived-backend 'ioslide 'html
  :menu-entry
  '(?i "Export to Google I/O HTML slide."
       ((?H "As HTML buffer" org-ioslide-export-as-html)
        (?h "As HTML file" org-ioslide-export-to-html))
       (?o "As HTML file and open"
           (lambda (a s v b)
             (if a (org-ioslide-export-to-html t s v b)
               (org-open-file (org-ioslide-export-to-html nil s v b))))))

  :filters-alist '((:filter-parse-tree . org-ioslide-generate-config-file))

  :options-alist
  '(
    ;; Overwrite the HTML_HEAD defined in ox-html.el
    (:html-head "HTML_HEAD" nil "" newline)

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
    ;; Note: the device must support touch.
    (:enable-touch      "ENABLE_TOUCH"      nil "true"  t)
    ;; favIcon
    (:fav-icon          "FAVICON"           nil "images/emacs-icon.png" t)
    (:hash-tag          "HASHTAG"           nil "" t)
    ;; TODO: fonts
    ;; Author information
    (:company           "COMPANY"           nil   nil   t)
    (:google-plus       "GOOGLE_PLUS"       nil   nil   t)
    (:twitter           "TWITTER"           nil   nil   t)
    (:www               "WWW"               nil   nil   t)
    (:github            "GITHUB"            nil   nil   t)

    ;; Other configs

    ;; Use MathJax, Default: true. False will remove MathJax from
    ;; current slide to save space (when exporting); True will
    ;; re-install it again.
    (:use-mathjax       "USE_MATHJAX"       nil "true"  t)
    ;; Google analytics: 'UA-XXXXXXXX-1
    (:analytics         "ANALYTICS"         nil   nil   t)
    (:logo              "LOGO"              nil    ""   t)
    (:icon              "ICON"              nil "images/emacs-icon.png" t)
    (:hlevel            "HLEVEL"            nil   nil   t)

    ;; TODO: idea ?
    ;; Hide the default title slide
    ;; (:hide-title-slide  "HIDE_TITLE_SLIDE" nil    nil   t)
    )

  :translate-alist
  '((headline                   .       org-ioslide-headline)
    (section                    .       org-ioslide-section)
    (template                   .       org-ioslide-template)
    (center-block               .       org-ioslide-center-block)
    (src-block                  .       org-ioslide-src-block)
    (quote-block                .       org-ioslide-quote-block)
    (verse-block                .       org-ioslide-verse-block)
    (table-cell                 .       org-ioslide-table-cell)
    (export-block               .       org-ioslide-export-block)
    (plain-list                 .       org-ioslide-plain-list)
    (paragraph                  .       org-ioslide-paragraph)
    (inner-template             .       org-ioslide-inner-template)
    (footnote-definition        .       org-ioslide-footnote-definition)
    (footnote-reference         .       org-ioslide-footnote-reference)
    )

  :export-block '("NOTE")
  )



;;; Internal Functions
(defun org-ioslide--plist-get-string (info key)
  (let ((r (plist-get info key)))
    (if (stringp r) r (or (car r) ""))))

(defun org-ioslide-close-element (element attr body)
  (format "<%s %s>\n%s\n</%s>" element attr body element))

(defun org-ioslide-close-element* (element attr body)
  "What is this?!"
  (format "</%s>\n%s\n<%s %s>\n" element body element attr))

(defun org-ioslide--copy-resource ()
  "Copy needed resource to current path."
  ;; TODO: make user use their own template instead of force copy
  ;; FIXME: do not copy image folder ?
  (mapc (lambda (dir)
          (copy-directory (f-join org-ioslide-path dir) dir))
        '("js/" "images/" "theme/")))

(defun org-ioslide-check-resource ()
  "Check js/slides.js exist or not, if not exist, re-fetch resource."
  (if (not (file-exists-p "js/slides.js"))
      (org-ioslide--copy-resource)))

(defun org-ioslide-generate-small-icon-css (icon-path hash-tag)
  "Generate theme/css/small-icon.css to overwrite style.
(The small icon at the left bottom corner)"
  (progn
    (save-excursion
      (with-temp-file "theme/css/small-icon.css"
        (insert "slides > slide:not(.nobackground):before {
background: url(../../" icon-path ") no-repeat 0 50%;
font-size: 12pt;
content: \"" hash-tag "\";
position: absolute;
bottom: 20px;
left: 60px;
-moz-background-size: 30px 30px;
-o-background-size: 30px 30px;
-webkit-background-size: 30px 30px;
background-size: 30px 30px;
padding-left: 40px;
height: 30px;
line-height: 1.9;
}")))
""))

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
           "     title: '%s', \n" (org-ioslide--plist-get-string info :title))
          ;; subtitle
          (format
           "     subtitle: '%s', \n" (org-ioslide--plist-get-string info :subtitle))
          ;; useBuilds
          (format
           "     useBuilds: %s, " (org-ioslide--plist-get-string info :use-builds))
          "// Default: true. False will turn off slide animation builds. \n"
          ;; usePrettify
          (format
           "     usePrettify: %s, " (org-ioslide--plist-get-string info :use-prettify))
          "// Default: true \n"
          ;; enableSlideAreas
          (format
           "     enableSlideAreas: %s, " (org-ioslide--plist-get-string info :enable-slideareas))
          "// Default: true. False turns off the click areas on either slide of the slides.\n"
          ;; enableTouch
          (format
           "     enableTouch: %s, " (org-ioslide--plist-get-string info :enable-touch))
          "// Default: true. If touch support should enabled. Note: the device must support touch.\n"
          ;; favIcon
          (format
           "     favIcon: '%s', \n" (org-ioslide--plist-get-string info :fav-icon))
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
           "     name: '%s', \n" (org-ioslide--plist-get-string info :author))
          ;; company
          (format
           "     company: '%s', \n" (org-ioslide--plist-get-string info :company))
          ;; google plus
          (format
           "     gplus: '%s', \n" (org-ioslide--plist-get-string info :google-plus))
          ;; twitter
          (format
           "     twitter: '%s', \n" (org-ioslide--plist-get-string info :twitter))
          ;; www
          (format
           "     www: '%s', \n" (org-ioslide--plist-get-string info :www))
          ;; github
          (format
           "     github: '%s', \n" (org-ioslide--plist-get-string info :github))

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

;;;; Quote Block
(defun org-ioslide-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((parent (org-export-get-parent-headline quote-block))
         (slide-prop (org-element-property :SLIDE parent))
         (attributes (org-export-read-attribute :attr_html quote-block))
         (class (plist-get attributes :class))
         (--make-sign (function
                       (lambda (string)
                         (replace-regexp-in-string
                          "^ *\\(&#x201[34];\\)\\(.+\\)\\(<br */>\\|\n\\)"
                          "<span class='alignright'>\\1\\2</span>\\3" string)))))
    (if (and class (string-match "notes?" class))
        (format "<aside class=\"note\">
  <section>
%s
  </section>
</aside>
" contents)
      (if (and slide-prop
               (string-match "segue" slide-prop))
          ;; [FIXME] different sign rendering under Firefox and Chrome...
          (format "<q>\n%s</q>"
                  (replace-regexp-in-string
                   "<br>\n *<span" "<span"
                   (replace-regexp-in-string
                    "</?p>" ""
                    (replace-regexp-in-string
                     "</?p>\n* *<p>" "<br>"
                     (funcall --make-sign contents)))))
        (format "<blockquote>\n%s</blockquote>"
                ;; Align "-- Name" to right side.
                (save-match-data
                  (replace-regexp-in-string
                   "</span>\n</p>"
                   "</span><br  />\n</p>"
                   (funcall --make-sign contents))))))))


;;; Verse Block
(defun org-ioslide-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Align "-- Name" to right side.
  (replace-regexp-in-string "^ *&#xa0;\\(?:&#xa0;\\)+\\(&#x201[34];\\)\\(.+?\\)\\(<br */>\\|\n\\)"
                            "<span class='alignright'>\\1\\2</span>\\3"
                            (org-html-verse-block verse-block contents info)))

;;;; Paragraph

(defun org-ioslide-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  ;; Fix multibyte language like chinese will be automatically add
  ;; some space since org-mode will transpose auto-fill-mode's space
  ;; to newline char.
  (let ((fix-contents
         (replace-regexp-in-string
          (concat "\\([[:multibyte:]]\\) *\n *\\([[:multibyte:]]\\)") "\\1\\2" contents)))

    ;; Send modify data to org-html-paragraph
    (org-html-paragraph paragraph fix-contents info)))

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
  (let ((user-id (plist-get info :analytics)))
    (if (null user-id)
        ""
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
               ) user-id))))

;;;; headline

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
         (full-text (if (fboundp 'org-html-format-headline--wrap)
                        (org-html-format-headline--wrap headline info)
                     (org-html-headline headline "" info))))
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
              (org-ioslide-begin-plain-list type))
         itemized-body
         (and (org-export-last-sibling-p headline info)
              (org-ioslide-end-plain-list type)))))

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
        (concat

         ;; Stop previous slide.
         ;; FIXME: This will make slide has more </slide> element
         (if (or (/= level 1)
                 (not (org-export-first-sibling-p headline info)))
             "</slide>\n")

         (org-ioslide-close-element
          (org-ioslide--container headline info)
          ;; container class
          (org-ioslide--container-class headline info)
          ;; body
          (format "%s%s%s"
                  ;; aside
                  (org-ioslide--aside headline info)
                  ;; title
                  (org-ioslide--title headline info)

                  ;; When there is no section, pretend there is an empty
                  ;; one to get the correct <div class="outline- ...>
                  ;; which is needed by `org-info.js'.
                  (if (not (eq (org-element-type first-content) 'section))
                      (concat (org-html-section first-content "" info)
                              "")
                    contents)
                  ))
         ))))))

(defun org-ioslide--container (headline info)
  "Return the top container of ioslide."
  "slide")

(defun org-ioslide--container-class (headline info)
  "Special handler for segue slide class."
  (let* ((class (org-element-property :SLIDE headline))
         (fill-image (org-element-property :FILL headline))
         (segue-p (or (string-match "segue" (format "%s" class) nil))))
    (format
     "id=\"%s\" %s %s"
     ;; id
     (or (org-element-property :CUSTOM_ID headline)
         (concat "sec-" (mapconcat 'number-to-string
                                   (org-export-get-headline-number headline info)
                                   "-")))
     (if (or segue-p class fill-image)
         (format "class=\"%s %s %s\""
                 (if fill-image "fill" "") class "nobackground") "")
     (if (or segue-p fill-image)
         (format "style=\"background-image: url(%s)\"" fill-image) "")
     )))

(defun org-ioslide--title (headline info)
  (let* ((title (format "%s " (or (org-element-property :TITLE headline) "")))
         (slide-prop (format "%s" (org-element-property :SLIDE headline)))
         (title-class (replace-regexp-in-string "\\<hide\\>" "" title))
         (hgroup-class (org-element-property :HGROUP headline)))
    (if (or (string-match "hide" title)
            (string-match "thank-you-slide" slide-prop))
        ""
      (format
       "<hgroup class=\"%s\">
       <h2 class=\"%s\">%s</h2>
       <h3>%s</h3>
       </hgroup>\n"
       ;; class
       (or hgroup-class "")
       ;; headline text.
       (or title-class "")
       (if (fboundp 'org-html-format-headline--wrap)
           (org-html-format-headline--wrap headline info)
         (org-html-headline headline "" info))
       ;; subtitle
       (or (org-element-property :SUBTITLE headline) "")))))

(defun org-ioslide--aside (headline info)
  (let* ((slide-class (format "%s" (org-element-property :SLIDE headline)))
         (segue-p (or (string-match "segue" slide-class) nil)))
    (if (< 0 (string-bytes slide-class))
        ;; icon
        (if segue-p
            (format
             "<aside class=\"gdbar %s\"><img src=\"%s\"></aside>"
             (or (org-element-property :ASIDE headline) "")
             ;; get ICON from property, if not exist get ICON from info
             (or (org-element-property :ICON headline)
                 (plist-get info :icon) "")
             ) "")
      "")))

(defun org-ioslide-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section. INFO is a plist
holding contextual information."
  ;; Just return the contents. No "<div>" tags.
  ;;  contents
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
                           (1- org-html-toplevel-hlevel)))
             (section-number
              (mapconcat
               'number-to-string
               (org-export-get-headline-number parent info) "-")))

        (org-ioslide-close-element
         "article"
         (format "class=\"%s\" id=\"text-%s\""
                 (or (org-element-property :ARTICLE parent) "")
                 (or (org-element-property :CUSTOM_ID parent) section-number))
         (if (string-match "thank-you-slide"
                           (format "%s" (org-element-property :SLIDE parent)))

             ;; Thank you slide
             (format
              "<h2>
  <p>%s</p>
</h2>
<br>
<p class=\"auto-fadein\" data-config-contact>
</p>"
              (org-export-data (org-element-property :title parent) info))

           ;; Normal content
           (format "%s\n%s"
                   (or contents "")
                   (org-ioslide--footer-from-footnote))
           ))))))

;; Footnotes

(defun org-ioslide--footer-from-footnote ()
  ""
  (if org-ioslide--current-footnote-list
      (prog1 (concat
              "<footer class=\"source\">\n"
              (mapconcat #'identity (reverse org-ioslide--current-footnote-list) "\n")
              "\n</footer>")
        ;; clean list
        (setq org-ioslide--current-footnote-list nil))
    ""))

(defun org-ioslide-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML (<footer class='source'>).
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   (cond
    ;; Do nothing if footnote has already been defined.
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     "")
    ;; Do nothing if reference is within another footnote
    ;; reference, footnote definition or table cell.
    ((cl-loop for parent in (org-export-get-genealogy footnote-reference)
              thereis (memq (org-element-type parent)
                            '(footnote-reference footnote-definition table-cell)))
     "")
    ;; Otherwise, add it into org-ioslide--current-footnote-list
    (t
     (let ((def (org-export-get-footnote-definition footnote-reference info)))
       (push
        (format "%s" (org-trim (org-export-data def info)))
        org-ioslide--current-footnote-list)
       ""
       )))))

(defun org-ioslide-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents
  )


;;;; Table Cell (support for "highlight" class)
(defun org-ioslide-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
         (table (org-export-get-parent-table table-cell))
         (cell-attrs
          (if (not org-html-table-align-individual-fields) ""
            (format " class=\"%s%s\""
                    (org-export-table-cell-alignment table-cell info)
                    (if (and (stringp contents)
                             (string-prefix-p "* " contents))
                        (progn (setq contents (substring contents 2))
                               " highlight")
                      "")
                    ))))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
           (= 1 (org-export-table-row-group table-row info)))
      (concat "\n" (format (car org-html-table-header-tags) "col" cell-attrs)
              contents (cdr org-html-table-header-tags)))
     ((and org-html-table-use-header-tags-for-first-column
           (zerop (cdr (org-export-table-cell-address table-cell info))))
      (concat "\n" (format (car org-html-table-header-tags) "row" cell-attrs)
              contents (cdr org-html-table-header-tags)))
     (t (concat "\n" (format (car org-html-table-data-tags) cell-attrs)
                contents (cdr org-html-table-data-tags))))))


;; Plain List

;; FIXME: Maybe arg1 is not needed because <li value="20"> already sets
;; the correct value for the item counter
(defun org-ioslide-begin-plain-list (type &optional class arg1)
  "Insert the beginning of the HTML list depending on TYPE.
When ARG1 is a string, use it as the start parameter for ordered
lists."
  (if class
      ;; quotes should be removed from "\"build fade\"", so:
      (setq class
            (format " class=\"%s\""
                    (replace-regexp-in-string "[\"']" "" class)))
    (setq class ""))
  (cl-case type
    (ordered
     (format "<ol%s%s>"
             class
             (if arg1 (format " start=\"%d\"" arg1) "")))
    (unordered (format "<ul%s>" class))
    (descriptive (format "<dl%s>" class))))

(defun org-ioslide-end-plain-list (type)
  "Insert the end of the HTML list depending on TYPE."
  (cl-case type
    (ordered "</ol>")
    (unordered "</ul>")
    (descriptive "</dl>")))

(defun org-ioslide-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attributes (org-export-read-attribute :attr_html plain-list))
         (class (plist-get attributes :class)))
    (format "%s\n%s%s"
            (org-ioslide-begin-plain-list type class)
            contents (org-ioslide-end-plain-list type))))



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
                     "utf-8")))
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
   (org-html-close-tag "link"
                       "rel=\"stylesheet\" media=\"all\" href=\"theme/css/small-icon.css\""
                       info)
   ;; [FIXME: ugly workaround] Generate theme/css/small-icon.css.
   (org-ioslide-generate-small-icon-css (org-ioslide--plist-get-string info :fav-icon)
                                        (org-ioslide--plist-get-string info :hash-tag))
   "\n"
   "<base target=\"_blank\"> <!-- This amazingness opens all links in a new tab. -->\n"
   "<script data-main=\"js/slides\" src=\"js/require-1.0.8.min.js\"></script>"
   "\n"))

(defun org-ioslide--build-head (info)
  "Return information for the <head>..</head> of the HTML output.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (org-element-normalize-string (plist-get info :html-head))
    (org-element-normalize-string (plist-get info :html-head-extra))
    (when (and (plist-get info :html-htmlized-css-url)
               (eq org-html-htmlize-output-type 'css))
      (org-html-close-tag "link"
                          (format " rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
                                  (plist-get info :html-htmlized-css-url))
                          info)))))

(defun org-ioslide--install-mathjax (info)
  "If '#+USE_MATHJAX: true' is set (default), install MathJax and enable it.
If '#+USE_MATHJAX: false' is set, remove MathJax directory to
save disk space."
  (if (string= "true" (org-ioslide--plist-get-string info :use-mathjax))
      (progn
        ;; Check if MathJax installed
        (if (not (file-exists-p "js/mathjax"))
            (copy-directory (concat org-ioslide-path "js/mathjax") "js/mathjax"))
        "\n<script src=\"js/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML,local/local\" type=\"text/javascript\"></script>\n")
    (progn
      (if (file-exists-p "js/mathjax")
          (delete-directory "js/mathjax" t))
      "")
    )
  )

(defun org-ioslide-template (contents info)
  "Return complete document string after HTML conversion.
contents is the transoded contents string.
info is a plist holding export options."
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

   ;; html head
   (org-ioslide--build-head info)

   "
   <script src=\"js/jquery-1.7.1.min.js\" type=\"text/javascript\"></script>
"

   ;; MathJax
   (org-ioslide--install-mathjax info)

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
      (switch-to-buffer-other-window outbuf)
      ;; Indent html buffer to make debug more easy
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max))
      (untabify (point-min) (point-max)))))

;;;###autoload
(defun org-ioslide-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Google ioslide HTML5 slide HTML file."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    ;; Check resource and re-fetch it
    (org-ioslide-check-resource)
    ;; export to html use ioslide backend
    (org-export-to-file
        'ioslide file subtreep visible-only body-only ext-plist)))


(provide 'ox-ioslide)
;;; ox-ioslide.el ends here
