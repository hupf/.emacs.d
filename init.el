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

;; Startup performance
(defun display-startup-time ()
  (message "⌛ Emacs loaded in %s with %d garbage collections"
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)


;; Configure `package'
(require 'package)

(setq package-enable-at-startup nil)
;; (package-initialize)
;; -> will already be called by auto-package-update

(setq package-archives
      '(;;("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(;;("melpa-stable" . 10)
        ("melpa" . 5)
        ("gnu" . 0))
      ;; package-pinned-packages
      ;; '((default-text-scale . "melpa")
      ;;   (centaur-tabs . "melpa")
      ;;   (flycheck . "melpa")
      ;;   (js2-mode . "melpa")
      ;;   (nvm . "melpa")
      ;;   (use-package . "melpa"))
)


;; Ensure diminish to be present to have use-package supporting it
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
;; (setq use-package-verbose t) ;; Debug package loading

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

(use-package f)

;; Automatically check for package updates
(use-package auto-package-update
  :init

  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-last-update-day-filename (f-join (file-relative-name no-littering-var-directory user-emacs-directory) "last-package-update-day"))

  :config
  (auto-package-update-maybe))

;; Setup directories and load initializers

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

;; Language-specific settings
(require 'initializer-lsp)
(require 'initializer-languages)
(require 'initializer-web)
(require 'initializer-javascript)
(require 'initializer-ruby)

;;; init.el ends here
