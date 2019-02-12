;;; initializer-css.el --- CSS editing

;;; Commentary:

;;; Configurations and modes for CSS and SCSS development.

;;; Code:

;; Color highlighting
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (css-mode scss-mode))

(use-package css-mode
  :ensure t
  :init (setq css-indent-offset 2)
  :mode "\\.css\\'")

(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'"
         "\\.sass\\'")

  :init
  (setq scss-compile-at-save nil) ;; Don't auto compile SASS files
  )

(provide 'initializer-css)
;;; initializer-css.el ends here
