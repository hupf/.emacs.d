;;; initializer-style.el --- Setup how Emacs looks.
;;;
;;; Commentary:
;;;
;;; There are three thing that make a good editor good: Capability,
;;; Performance and Style.  Each of which is critical.  It's the last of
;;; these, Style, that this module is all about.
;;;
;;; Let's make it look good!
;;;
;;; Code:


;; Theme
(use-package monokai-theme
  :ensure t

  :init
  (setq custom--inhibit-theme-enable nil) ;; Required for Emacs 27 to be able to customize theme faces below

  :config
  (load-theme 'monokai t)
  (custom-theme-set-faces
     'monokai
     `(font-lock-comment-face ((t (:slant italic :foreground ,monokai-comments)))))
  )


;; Fonts
(when (eq window-system 'x)
  ;; (set-frame-font "Monospace-11")
  ;; (set-frame-font "IBM Plex Mono 11")
  (set-frame-font "Victor Mono Light 12")
)
(when (eq system-type 'darwin)
  ;; (set-frame-font "Menlo 14")
  (set-frame-font "IBM Plex Mono Light 14")
  ;; (set-frame-font "IBM Plex Mono 14")
  )

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(provide 'initializer-style)
;;; initializer-style.el ends here
