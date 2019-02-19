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
  :init (setq monokai-distinct-fringe-background t)
  :config (load-theme 'monokai t))


;; Fonts
(when (eq window-system 'x)
  (set-default-font "Monospace-11")
  ;;(set-default-font "IBM Plex Mono 11")

)
(when (eq system-type 'darwin)
  ;; (set-default-font "Menlo 14")
  (set-default-font "IBM Plex Mono Light 14")
  ;; (set-default-font "IBM Plex Mono 14")
  )

(set-face-attribute 'fringe nil
  :background (face-background 'default))

;; (custom-set-faces
;;  '(fringe ((t (:background "#20211d"))))
;;  '(git-gutter:unchanged ((t (:background "#20211d")))))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(provide 'initializer-style)
;;; initializer-style.el ends here
