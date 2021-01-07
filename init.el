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
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("melpa" . 5)
        ("gnu" . 0))
      package-pinned-packages
      '((default-text-scale . "melpa")
        (centaur-tabs . "melpa")
        (flycheck . "melpa")
        (js2-mode . "melpa")
        (nvm . "melpa")
        (use-package . "melpa")))


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

;; Bootstrap the `straight` package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Keep ~/.emacs.d clean, by putting config files into ~/.emacs.d/etc
;; and temporary files (including backup and auto-save files) into
;; ~/.emacs.d/var
(use-package no-littering
  :ensure t

  :config
  ;; No-littering does not change auto saves directory, change it to var directory
  (setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; By default Emacs poops all customizations set through the
;; customization UI into your `init.el'. Let's not do that.
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; Setup directories and load initializers
(use-package f
  :ensure t)

(defvar initializers-directory (f-join user-emacs-directory "initializers")
  "All .el files in this directory will be run.")

(add-to-list 'load-path initializers-directory)

;; load all of the files in the `initializers/' directory.
;; (eval '(mapc 'load (directory-files initializers-directory 't "^[^#.].*el$")))

(require 'initializer-helpers)
(require 'initializer-style)
(require 'initializer-system)
(require 'initializer-completion)
(require 'initializer-projectile)
(require 'initializer-vcs)
(require 'initializer-windowing)
(require 'initializer-editing)
(require 'initializer-keys)
(require 'initializer-modeline)
(require 'initializer-treemacs)

;; different language modes
(require 'initializer-lsp)
(require 'initializer-web)
(require 'initializer-javascript)
(require 'initializer-css)
(require 'initializer-ruby)
(require 'initializer-yaml)
(require 'initializer-markdown)
(require 'initializer-asciidoc)

;;; init.el ends here
