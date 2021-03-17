;;; initializer-lsp --- Setup modeline
;;;
;;; Commentary:
;;;
;;; Setup of the modeline look and behavior
;;;
;;; Code:

;; Display column number in modeline
(column-number-mode t)

;; Replace the annoying bell ring with modeline flash
(use-package mode-line-bell
  :config (mode-line-bell-mode))

;; Fonts & colors
(defvar modeline-font (concat modeline-font-name " " (number-to-string modeline-font-size)))
(let ((faces '(mode-line
               mode-line-buffer-id
               mode-line-emphasis
               mode-line-highlight
               mode-line-inactive)))
  (mapc
   (lambda (face)
     (set-face-attribute face nil :font modeline-font)
   )
   faces))
(set-face-attribute 'mode-line nil :background "#21211d" :box '(:line-width 8 :color "#21211d") :overline nil :underline nil)
(set-face-attribute 'mode-line-inactive nil :background "#252520" :box '(:line-width 8 :color "#252520"))

;; Remove Git branch from modeline
(setq mode-line-format (delq (assoc 'vc-mode mode-line-format) mode-line-format))

(provide 'initializer-modeline)
;;; initializer-modeline.el ends here
