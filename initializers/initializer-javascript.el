;;; initializer-javascript.el --- Setup JavaScript editing -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Modes and configurations for JavaScript and TypeScript editing.
;;;
;;; Code:

;; Mise version manager
(use-package mise
  :hook (after-init . global-mise-mode))

(use-package add-node-modules-path

  :custom
  ;; Workaround for removed `npm bin' command in npm v9
  ;; (see https://github.com/codesuki/add-node-modules-path/issues/23)
  (add-node-modules-path-command "echo \"$(npm root)/.bin\"")

  :commands (add-node-modules-path))


;; JavaScript
(use-package js-ts-mode
  :ensure nil
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.jsx\\'")

  :init
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq flycheck-disabled-checkers '(javascript-jshint)) ;; Disable JSHint

  :hook
  (js-ts-mode . setup-javascript-lsp))

;; TypeScript
(use-package typescript-ts-mode
  :ensure nil
  :mode
  (("\\.ts\\'" . typescript-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode))

  :hook
  ((typescript-ts-mode tsx-ts-mode) . setup-javascript-lsp))

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

;; JSON
(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'")

(provide 'initializer-javascript)
;;; initializer-javascript.el ends here
