#+TITLE: org-ioslide

# Badge
[[http://www.gnu.org/licenses/gpl-3.0.txt][https://img.shields.io/badge/license-GPL_3-green.svg?dummy]]
[[https://travis-ci.org/coldnew/org-ioslide][https://travis-ci.org/coldnew/org-ioslide.svg?branch=master]]
[[http://melpa.org/#/ox-ioslide][http://melpa.org/packages/ox-ioslide-badge.svg]]
[[http://stable.melpa.org/#/ox-ioslide][http://stable.melpa.org/packages/ox-ioslide-badge.svg]]
[[http://waffle.io/coldnew/org-ioslide][https://badge.waffle.io/coldnew/org-ioslide.png?label=ready&title=Ready]]

Export your Org document to Google I/O HTML5 slide.
See [[http://coldnew.github.io/org-ioslide][DEMO]] slide.

#+BEGIN_QUOTE
Note: Org 8.x or above is required.
#+END_QUOTE

* Installation

If you have =melpa= and =emacs24= installed, simply type:

: M-x package-install ox-ioslide

And following code in your .emacs

#+BEGIN_SRC emacs-lisp
  (require 'ox-ioslide)
#+END_SRC

For =cask= user, just add following lines in your =Cask= file

#+BEGIN_SRC emacs-lisp
  (source melpa)

  (depends-on "ox-ioslide")
#+END_SRC

If you want to install manually, just do as following:

=git clone= this repository into =whatever/=, then in your Emacs init file:

#+BEGIN_SRC emacs-lisp
  (add-to-list 'load-path "path/to/whatever/")
  (require 'ox-ioslide)
#+END_SRC

* Usage

Just take a look of [[file:example/index.html]] and
[[file:example/index.org]], which contains detailed examples & demo.

** If Tired Of Memorize Magic Code...
=ox-ioslide-helper.el= is also available to help you interactively inserting verbose properties/options/attributes into your slide file. 

#+BEGIN_QUOTE
This package depends on =makey.el=, install it if not.
#+END_QUOTE

Just =(require 'ox-ioslide-helper)= then =M-x ioslide:helper=.

* Development Info
** Editing CSS

[[http://compass-style.org/install/][Compass]] is a CSS preprocessor used to compile
SCSS/SASS into CSS. We chose SCSS for the new slide deck for maintainability,
easier browser compatibility, and because...it's the future!

That said, if not comfortable working with SCSS or don't want to learn something
new, not a problem. The generated .css files can already be found in
(see [[theme/css)][/theme/css]] You can just edit those and bypass SCSS altogether.
However, our recommendation is to use Compass. It's super easy to install and use.

*** Installing Compass and making changes

First, install compass:

: sudo gem update --system
: sudo gem install compass

Next, you'll want to watch for changes to the exiting .scss files in [[theme/scss][/theme/scss]]
and any new one you add:

: $ cd io-2012-slides
: $ compass watch

This command automatically recompiles the .scss file when you make a change.
Its corresponding .css file is output to [[theme/css][/theme/css]]. Slick.

By default, [[config.rb][config.rb]] in the main project folder outputs minified
.css. It's a best practice after all! However, if you want unminified files,
run watch with the style output flag:

    compass watch -s expanded

*Note:* You should not need to edit [[theme/scss/_base.scss][_base.scss]].
