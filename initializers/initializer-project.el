;;; initializer-project.el --- Setup project.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Configuration of the built-in project.el project interaction library.
;;;
;;; Code:

(use-package project
  :bind-keymap
  ("C-c p" . project-prefix-map)

  :bind
  ("C-S-t" . project-find-file)

  :config
  ;; Richer buffer picker (recent files + open buffers) than the
  ;; default `project-switch-to-buffer'
  (define-key project-prefix-map "b" #'consult-project-buffer)
  ;; Magit instead of the built-in `vc-dir' UI
  (define-key project-prefix-map "v" #'magit-status-with-node)

  ;; `project-switch-project' (C-c p p) has its own dispatch menu
  ;; restricted to this list, separate from `project-prefix-map' —
  ;; keep its entries in sync with the overrides above
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (magit-status-with-node "Magit")
          (project-eshell "Eshell")
          (project-any-command "Other"))))

(provide 'initializer-project)
;;; initializer-project.el ends here
