;;; initializer-css.el --- Setup CSS editing
;;;
;;; Commentary:
;;;
;;; Modes and configurations for CSS/SCSS editing.
;;;
;;; Code:

;; Color highlighting
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (css-mode scss-mode))

(use-package css-mode
  :ensure t
  :init (setq css-indent-offset 2)
  :mode "\\.css\\'"
  :hook (css-mode . setup-node-lsp))

(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'"
         "\\.sass\\'")
  :hook (scss-mode . setup-node-lsp)

  :init
  (setq scss-compile-at-save nil) ;; Don't auto compile SASS files
  )

(provide 'initializer-css)
;;; initializer-css.el ends here
