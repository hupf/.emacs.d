;;; initializer-lsp --- Setup LSP
;;;
;;; Commentary:
;;;
;;; Setup of the language server protocol
;;;
;;; Code:

;; TODO: Activate project's node version via nvm.el?
(setq exec-path (append exec-path '("~/.nvm/versions/node/v14.15.0/bin")))

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

;; TODO: Auto install `npm i -g typescript-language-server` if not present?
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; css/scss
;; TODO: Auto install `npm install -g vscode-css-languageserver-bin` if not present?

;; html
;; TODO: Auto install `npm install -g vscode-html-languageserver-bin` if not present?

;; angular
;; TODO: Auto install `npm install -g @angular/language-service@next typescript @angular/language-server` if not present?
;; TODO: does not work yet, configure lsp-clients-angular-language-server-command? -> with correct node directory?

(provide 'initializer-lsp)
;;; initializer-lsp.el ends here
