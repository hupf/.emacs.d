;;; initializer-css.el --- Setup CSS editing
;;;
;;; Commentary:
;;;
;;; Modes and configurations for CSS/SCSS editing.
;;;
;;; Code:

(use-package css-mode
  :init (setq css-indent-offset 2)
  :mode "\\.css\\'"
  :hook (css-mode . setup-node-lsp))

(use-package scss-mode
  :mode ("\\.scss\\'"
         "\\.sass\\'")
  :hook (scss-mode . setup-node-lsp)

  :init
  ;; Don't auto compile SASS files
  (setq scss-compile-at-save nil))

;; Color highlighting
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (css-mode scss-mode))

(provide 'initializer-css)
;;; initializer-css.el ends here
