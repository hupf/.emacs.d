;; -*- lexical-binding: t -*-
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

;; Whenever Emacs loads some elisp that is not compiled yet, compile it and load it
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; Disable annoying warnings in native comp Emacs
(setq native-comp-async-report-warnings-errors nil)

;; When launching Emacs from GUI: determine `exec-path' & environment
;; variables by starting an actual shell session
(use-package exec-path-from-shell
  :custom
  ;; Environment variables to be copied from the shell
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GEM_PATH" "GOROOT"))

  ;; Since starting a shell session is quite time consuming
  ;; (`exec-path-from-shell-initialize' slows down Emacs startup by a
  ;; few seconds), call the custom
  ;; `exec-path-from-shell-initialize-maybe' function below, before
  ;; executing something that relies on it...
  ;; :config (exec-path-from-shell-initialize)
  )

(setq exec-path-from-shell--initialized-p nil)
(defun exec-path-from-shell-initialize-once ()
  "Initialize 'exec-path', but only when called the first time."
  (when (not exec-path-from-shell--initialized-p)
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell--initialized-p t)))

;; Don't open new dired buffer when visiting directory in dired buffer
(setq dired-kill-when-opening-new-dired-buffer t)

;; OSX specifics
(when (eq system-type 'darwin)
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
