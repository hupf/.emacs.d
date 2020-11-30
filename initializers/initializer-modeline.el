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
  :ensure t
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
     (set-face-attribute face nil :font modeline-font :background "#20211c" :box '(:line-width 8 :color "#20211c") :overline nil :underline nil)
   )
   faces))
(set-face-attribute 'mode-line-inactive nil  :background "#252620" :box '(:line-width 8 :color "#252620"))

(provide 'initializer-modeline)
;;; initializer-modeline.el ends here
