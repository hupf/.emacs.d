;;; initializer-system.el --- System level tuning
;;;
;;; Commentary:
;;;
;;; Some stuff is not really anything to do with editing or modifying
;;; code, but related to the performance of Emacs itself, or where it
;;; can find executables in the operating system, or how something will
;;; behave on a Mac vs a Linux or Windows box.  Those types of tweaks go
;;; here.
;;;
;;; This is ported from Prelude.
;;;   https://github.com/bbatsov/prelude/blob/master/core/prelude-osx.el
;;;
;;; Code:

;; forward function declarations eliminate warnings about whether a
;; function is defined.
;; (declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")

;; reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)

;; Increase the amount of data which Emacs reads from a process
;; (useful for LSP)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; disable the toolbar at the top of the window
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GEM_PATH"))

;; OSX specific code
(when (eq system-type 'darwin)

  ;; On OS X Emacs doesn't use the shell PATH if it's not started from
  ;; the shell. Let's fix that:
  ;; (use-package exec-path-from-shell
  ;;   :ensure t
  ;;   :config
  ;;   (exec-path-from-shell-initialize))

  ;; Fix keys
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)

  ;; proced-mode doesn't work on OS X so we use vkill instead
  (autoload 'vkill "vkill" nil t)
  (global-set-key (kbd "C-x p") 'vkill)

  (menu-bar-mode +1)

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

(provide 'initializer-system)
;;; initializer-system.el ends here
