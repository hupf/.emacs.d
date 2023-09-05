;;; initializer-lsp --- Setup LSP
;;;
;;; Commentary:
;;;
;;; Setup of the language server protocol
;;;
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)

  ;; :hook
  ;; (prog-mode . lsp-deferred))
  ;; (prog-mode . setup-node-lsp)

  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)

  :config
  (lsp-enable-which-key-integration t)

  ;; Add custom language configurations where not working per default
  (setq lsp-language-id-configuration (append lsp-language-id-configuration '(
    (web-mode . "html") ;; Make sure the html server is used for .hbs templates
  ))))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable nil) ;; Sideline is good for wide buffers, I prefer flycheck errors in echo area

  :hook
  (lsp-mode . lsp-ui-mode))

(defun setup-node-lsp ()
  "Initialize LSP after enabling local Node.js version."
  ;; Use project's node version
  (nvm-use-for-buffer)

  ;; Allow to access project's eslint
  (add-node-modules-path)

  ;; Somehow the Angular language server cannot be found, so explcitly
  ;; define its server command here
  (setq lsp-clients-angular-language-server-command
        (let ((node-modules-path (concat (cadr nvm-current-version) "/lib/node_modules")))
          `("node"
            ,(concat node-modules-path "/@angular/language-server")
            "--ngProbeLocations" ,node-modules-path
            "--tsProbeLocations" ,node-modules-path
            "--stdio")))

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
