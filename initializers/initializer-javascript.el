;;; initializer-javascript.el --- Setup JavaScript editing
;;;
;;; Commentary:
;;;
;;; Modes and configurations for JavaScript and TypeScript editing.
;;;
;;; Code:

;; Node version management
(use-package nvm
  :ensure t
  :commands (nvm-use nvm-use-for))

(defun nvm-use-for-buffer ()
  "Activate Node based on an .nvmrc for the current file.
If buffer is not visiting a file, do nothing."
  (when buffer-file-name
    (condition-case err
        (nvm-use-for buffer-file-name)
      (error (message "%s" err)))))

(use-package add-node-modules-path
  :ensure t
  :commands (add-node-modules-path))


;; JavaScript
(use-package js-mode
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.jsx\\'")

  :init
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq flycheck-disabled-checkers '(javascript-jshint)) ;; Disable JSHint

  :hook
  (js-mode . setup-javascript-lsp))

;; (use-package rjsx-mode
;;   :ensure t

;;   :mode ("\\.js\\'" "\\.mjs\\'")
;;   :interpreter "node"

;;   :init
;;   (setq js-indent-level 2)
;;   (setq js2-basic-offset 2)
;;   (setq js2-highlight-level 3)
;;   (setq js2-mode-show-parse-errors nil)
;;   (setq js2-mode-show-strict-warnings nil)
;;   (setq js2-strict-missing-semi-warning nil)
;;   (setq js2-missing-semi-one-line-override t)
;;   (setq js2-indent-switch-body t)
;;   (setq js-switch-indent-offset 2)

;;   ;; Disable JSHint
;;   (setq flycheck-disabled-checkers '(javascript-jshint))

;;   :hook (;;(js2-mode . setup-js2)
;;          (js2-mode . setup-node-lsp))
;;   )

;; (defun setup-js2 ()
;;   ;; Use project's node version
;;   (nvm-use-for-buffer)

;;   ;; Allow to access project's eslint
;;   (add-node-modules-path)

;;   ;; To workaround messages outputted by eslint on STDERR,
;;   ;; use a wrapper script that ignore STDERR
;;   (make-local-variable 'exec-path)
;;   (add-to-list 'exec-path "~/bin")

;;   ;; Put this script into ~/bin/eslint:
;;   ;; #!/usr/bin/env bash
;;   ;; PROJECT_ROOT=$(pwd -P 2>/dev/null || command pwd)
;;   ;; while [ ! -e "$PROJECT_ROOT/node_modules" ]; do
;;   ;; PROJECT_ROOT=${PROJECT_ROOT%/*}
;;   ;; if [ "$PROJECT_ROOT" = "" ]; then break; fi
;;   ;; done
;;   ;; ESLINT=$PROJECT_ROOT/node_modules/.bin/eslint
;;   ;; $ESLINT "$@" 2> /dev/null

;;   (defun has-ancestor-file (file-name)
;;     (let* ((current-directory (expand-file-name (buffer-file-name (current-buffer))))
;;           (path (locate-dominating-file current-directory file-name)))
;;       (> (length path) 0)))

;;   ;; Enable ESLint if configuration file is available to avoid
;;   ;; flycheck error
;;   (if (or (has-ancestor-file ".eslintrc.js")
;;           (has-ancestor-file ".eslintrc.yaml")
;;           (has-ancestor-file ".eslintrc.json")
;;           (has-ancestor-file ".eslintrc"))
;;       (flycheck-select-checker 'javascript-eslint))
;;   )

;; TODO
;; (require 'js2-refactor)
;; (require 'js-doc)

;; ;; We use js2r-refactor-mode which implies using js2-mode.
;; ;; see https://github.com/magnars/js2-refactor.el
;; ;;
;; ;; all refactorings start with C-c C-r (for refactor!)
;; (js2r-add-keybindings-with-prefix "C-c C-r")
;; (add-hook 'js2-mode-hook 'js2-refactor-mode)


;; ;; setup jsdoc: https://github.com/mooz/js-doc
;; ;;
;; ;; We use the same prefix for js2r `C-c C-r' because it's an "advanced"
;; ;; refactory-y type thing. The additional `i' prefix is for "insert"
;; (define-key js2-refactor-mode-map (kbd "C-c C-r i d") #'js-doc-insert-function-doc)
;; (define-key js2-refactor-mode-map "@" #'js-doc-insert-tag)

;; ;;; parse node.js stack traces in compilation buffer.s
;; (require 'compile)
;; (add-to-list 'compilation-error-regexp-alist 'node)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))

;; TypeScript
(use-package typescript-mode
  :ensure t

  :mode
  ("\\.ts\\'" "\\.tsx\\'")

  :hook
  (typescript-mode . setup-javascript-lsp)

  :config
  (setq typescript-indent-level 2))

(defun setup-javascript-lsp ()
  "Initialize LSP and enable the ESLint checker."
  (setup-node-lsp)

  ;; Make sure the ESLint checker is executed in chain with the LSP
  ;; checker. Use this hacky timer solution for now, since it somehow
  ;; doesn't work inline, even when used after calling `lsp`, not
  ;; `lsp-deferred` in the above `setup-node-lsp` function.
  (run-with-timer 2 nil (lambda ()
                          (when (member 'lsp flycheck-checkers)
                              (flycheck-add-next-checker 'lsp 'javascript-eslint)))))

;; CoffeeScript
(use-package coffee-mode
  :ensure t

  :mode "\\.coffee\\'"

  :init
  (setq coffee-tab-width 2)
  (setq coffee-indent-tabs-mode nil))

;; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(provide 'initializer-javascript)
;;; initializer-javascript.el ends here
