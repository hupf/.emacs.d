;; Color highlighting
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (css-mode scss-mode))

(use-package css-mode
  :ensure t
  :init (setq css-indent-offset 2)
  :mode ("\\.css\\'"
         "\\.scss\\'"
         "\\.sass\\'")
)

;; TODO: scss-mode?
;; (defun configure-auto-complete-for-scss ()
;;   (add-to-list 'ac-sources 'ac-source-css-property))
;; (add-hook 'scss-mode-hook 'configure-auto-complete-for-scss)
;; (add-to-list 'ac-modes 'scss-mode)

;; (setq scss-compile-at-save nil) ;; don't auto compile sass files

(provide 'initializer-css)
