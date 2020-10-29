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

(provide 'initializer-modeline)
;;; initializer-modeline.el ends here
