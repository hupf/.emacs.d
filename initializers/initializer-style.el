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
  :init
  (setq custom--inhibit-theme-enable nil) ;; Required for Emacs 27 to be able to customize theme faces below

  :config
  (load-theme 'monokai t)

  ;; Make comments italic
  (custom-theme-set-faces
     'monokai
     `(font-lock-comment-face ((t (:slant italic :foreground ,monokai-comments))))))


;; Fonts
(setq frame-font-name "Victor Mono Light")
(setq frame-font-size (if (eq window-system 'x)
    12 ; Smaller font on Linux
    14))

(setq treemacs-font-name "IBM Plex Sans Light")
(setq treemacs-font-size (if (eq window-system 'x)
    (- frame-font-size 1) ; Smaller font on Linux
    frame-font-size))

(setq modeline-font-name "IBM Plex Sans")
(setq modeline-font-size (if (eq window-system 'x)
    (+ frame-font-size 1) ; Smaller font on Linux
    (+ frame-font-size 2)))

(set-frame-font (concat frame-font-name " " (number-to-string frame-font-size)))


;; Emojis
(set-fontset-font t 'symbol "Noto Color Emoji")
(set-fontset-font t 'symbol "Apple Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(provide 'initializer-style)
;;; initializer-style.el ends here
