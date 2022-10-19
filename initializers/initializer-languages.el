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

;; Haskell
(defun setup-haskell-mode ()
  "Initialize haskell-mode."
  (setq-local tab-width 2)

  (exec-path-from-shell-initialize-once)

  ;; Install https://www.haskell.org/ghcup/ with language server
  (use-package lsp-haskell)
  (lsp-deferred))
(use-package haskell-mode
  :mode "\\.hs"

  :config


  :hook
  (haskell-mode . setup-haskell-mode)
  (haskell-literate-mode . setup-haskell-mode))

;; Go
(defun setup-go-mode ()
  "Initialize go-mode."
  (setq-local tab-width 2)

  ;; Auto format and import on save
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)

  (exec-path-from-shell-initialize-once)
  ;; Install gopls server: https://github.com/golang/tools/tree/master/gopls#installation
  (lsp-deferred))

(use-package go-mode
  :mode ("\\.go\\'")
  :mode ("\\.mod\\'")
  :hook (go-mode . setup-go-mode))

;; PHP
(use-package php-mode
  :mode "\\.php\\'")

;; Elm
;; Make sure to have elm installed: https://guide.elm-lang.org/install/elm.html
;; And elm-format via `npm i -g elm-format`
(use-package elm-mode
  :mode "\\.elm\\'"
  :hook
  (elm-mode . elm-format-on-save-mode)
  (elm-mode . setup-node-lsp))

;; (use-package flycheck-elm
;;   :after elm-mode flycheck
;;   :hook
;;   (mode-hook . #'flycheck-elm-setup))

(provide 'initializer-languages)
;;; initializer-languages.el ends here
