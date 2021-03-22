;;; initializer-languages.el --- Setup various languages
;;;
;;; Commentary:
;;;
;;; Modes and configurations for various code languages.
;;;
;;; Code:

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; Asciidoc
(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))

;; YAML
(use-package yaml-mode
  :mode "\\.yml\\'")

;; Go
(defun setup-go-mode ()
  "Initialize go-mode, when ready."
  (setq-local tab-width 2)

  ;; Auto format and import on save
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)

  (exec-path-from-shell-initialize-maybe)
  ;; Install gopls server: https://github.com/golang/tools/tree/master/gopls#installation
  (lsp-deferred))

(use-package go-mode
  :mode ("\\.go\\'")
  :mode ("\\.mod\\'")
  :hook (go-mode . setup-go-mode))

;; PHP
(use-package php-mode
  :mode "\\.php\\'")

(provide 'initializer-languages)
;;; initializer-languages.el ends here
