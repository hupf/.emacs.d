;;; initializer-lsp --- Setup LSP -*- lexical-binding: t; -*-
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
  (lsp-completion-provider :none) ;; Disable company to use corfu

  :config
  (lsp-enable-which-key-integration t)

  ;; Add custom language configurations where not working per default
  (setq lsp-language-id-configuration (append lsp-language-id-configuration '(
    (web-mode . "html") ;; Make sure the html server is used for .hbs templates
    )))

  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable nil) ;; Sideline is good for wide buffers, I prefer flycheck errors in echo area

  :hook
  (lsp-mode . lsp-ui-mode))

(defun my/mise-tool-node-modules-dir (tool)
  "Return the `lib/node_modules' directory of mise-managed npm TOOL.
Unlike a shared global npm prefix, mise's `npm:' backend installs
each tool into its own isolated directory, so every tool's
node_modules must be resolved individually."
  (let ((dir (string-trim (shell-command-to-string (concat "mise where " tool)))))
    (unless (string-empty-p dir)
      (expand-file-name "lib/node_modules" dir))))

(defun setup-node-lsp ()
  "Initialize LSP after enabling local Node.js version."
  ;; Activate mise early so exec-path is populated before executable-find below.
  ;; global-mise-mode hooks into after-change-major-mode-hook, which runs after
  ;; mode hooks, so we need to trigger it explicitly here.
  (mise-mode 1)

  ;; Allow to access project's eslint
  (add-node-modules-path)

  ;; Determine the location of the Angular language server
  ;; This assumes the following entries in the .config/mise/config.toml:
  ;;   [tools]
  ;;   "npm:@angular/language-server" = "latest"
  ;;   "npm:typescript" = "latest"
  ;;   "npm:typescript-language-server" = "latest"
  (when-let* ((ng-node-modules (my/mise-tool-node-modules-dir "npm:@angular/language-server"))
              (ts-node-modules (my/mise-tool-node-modules-dir "npm:typescript")))
    ;; `@angular/language-service' must match the project's own Angular
    ;; version, so it lives in the project's node_modules rather than in
    ;; mise's isolated angular-language-server install; probe there first.
    (let* ((project-root (locate-dominating-file default-directory "node_modules"))
           (project-node-modules (and project-root (expand-file-name "node_modules" project-root)))
           (ng-probe-locations (if project-node-modules
                                   (list project-node-modules ng-node-modules)
                                 (list ng-node-modules))))
      (setq lsp-clients-angular-language-server-command
            `("node"
              ,(expand-file-name "@angular/language-server" ng-node-modules)
              "--ngProbeLocations" ,(string-join ng-probe-locations ",")
              "--tsProbeLocations" ,ts-node-modules
              "--stdio"))))

  ;; Enable lsp-mode once buffer is visible
  ;; (lsp-deferred)

  ;; Instead of the above deferred variant, enable it directly for
  ;; now, to fix the enabling of the ESLint checker in
  ;; `initializer-javascript`
  (lsp))

;; angular
;; TODO: Auto install `npm install -g @angular/language-service@next typescript @angular/language-server` if not present?
;; TODO: does not work yet, configure lsp-clients-angular-language-server-command? -> with correct node directory?

(use-package lsp-tailwindcss
  :ensure nil
  :custom
  (lsp-tailwindcss-add-on-mode t))

(provide 'initializer-lsp)
;;; initializer-lsp.el ends here
