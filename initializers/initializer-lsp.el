;;; initializer-lsp --- Setup LSP
;;;
;;; Commentary:
;;;
;;; Setup of the language server protocol
;;;
;;; Code:

(setq exec-path (append exec-path '("~/.nvm/versions/node/v11.9.0/bin")))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)

  ;; :hook
  ;; (prog-mode . lsp-deferred))
  ;; (prog-mode . setup-lsp)

  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'

  :config
  (lsp-enable-which-key-integration t))

;; (defun setup-lsp ()
;;   (nvm-use-for-buffer)
;;   (lsp-deferred))

(use-package lsp-ui
  :ensure t

  :hook
  (lsp-mode . lsp-ui-mode)

  :config
  (setq lsp-ui-doc-position 'bottom)
  )

(use-package lsp-ivy
  :ensure t)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(provide 'initializer-lsp)
;;; initializer-lsp.el ends here
