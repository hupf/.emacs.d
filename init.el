;;; init.el --- Configuration and storage.
;;;
;;; Commentary:
;;;
;;; Loads all of the elisp files in .emacs.d/initializers.
;;;
;;; .emacs.d/initalizers/ will be created for you if it doesn't exist
;;; already.
;;;
;;; Code:


;; To measure Emacs startup time, execute:
;; $ time emacs -l init.el -batch --eval '(message "Hello, world!")'

;; Configure `package'
(require 'package)

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/")
        ("GNU" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("MELPA" . 5)
        ("GNU" . 0))
      package-pinned-packages
      '((js2-mode . "MELPA")
        (use-package . "MELPA")))


;; Ensure diminish to be present to have use-package supporting it
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Bootstrap `quelpa' for zoom-frm
;; (use-package quelpa-use-package
;;   :ensure t)

;; Setup directories and load initializers
(use-package f
  :ensure t)

(defvar user-data-directory (f-join user-emacs-directory "data")
  "This directory contains persistent application state.
The data directory is for storing things like autosave files and recent lists so
that they don't get pooped into things like your home directory or your init
file, but instead go to a well-known location.")

;; create the data directory if it doesn't already exist.
(unless (f-exists? user-data-directory)
  (f-mkdir user-data-directory))

;; by default Emacs poops all customizations set through the
;; customization UI into your `init.el'. Let's not do that.
(setq custom-file (f-join user-data-directory "custom.el"))

(defvar initializers-directory (f-join user-emacs-directory "initializers")
  "All .el files in this directory will be run.")

(add-to-list 'load-path initializers-directory)

;; load all of the files in the `initializers/' directory.
;; (eval '(mapc 'load (directory-files initializers-directory 't "^[^#.].*el$")))

(require 'initializer-helpers)
(require 'initializer-style)
(require 'initializer-system)
(require 'initializer-projectile)
(require 'initializer-completion)
(require 'initializer-vcs)
(require 'initializer-windowing)
(require 'initializer-editing)
(require 'initializer-keys)
(require 'initializer-modeline)
(require 'initializer-treemacs)

;; different language modes
(require 'initializer-web)
(require 'initializer-javascript)
;; (require 'initializer-typescript)
(require 'initializer-lsp)
(require 'initializer-css)
(require 'initializer-ruby)
(require 'initializer-yaml)
(require 'initializer-markdown)
(require 'initializer-asciidoc)

;;; init.el ends here
