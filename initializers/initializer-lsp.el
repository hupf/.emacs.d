;;; initializer-lsp --- Setup LSP
;;;
;;; Commentary:
;;;
;;; Setup of the language server protocol
;;;
;;; Code:

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)

  ;; :hook
  ;; (prog-mode . lsp-deferred))
  ;; (prog-mode . setup-node-lsp)

  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'

  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t

  :hook
  (lsp-mode . lsp-ui-mode)

  :config
  (setq lsp-ui-doc-position 'bottom))

(defun setup-node-lsp ()
  "Initialize LSP after enabling local Node.js version."
  ;; Use project's node version
  (nvm-use-for-buffer)

  ;; Allow to access project's eslint
  (add-node-modules-path)

  ;; Enable lsp-mode once buffer is visible
  ;; (lsp-deferred)

  ;; Instead of the above deferred variant, enable it directly for
  ;; now, to fix the enabling of the ESLint checker in
  ;; `initializer-javascript`
  (lsp))

;; angular
;; TODO: Auto install `npm install -g @angular/language-service@next typescript @angular/language-server` if not present?
;; TODO: does not work yet, configure lsp-clients-angular-language-server-command? -> with correct node directory?

(provide 'initializer-lsp)
;;; initializer-lsp.el ends here
