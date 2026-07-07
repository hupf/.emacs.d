;;; initializer-vcs.el --- Version control tools -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Utilities and tools for version control
;;;
;;; Code:

(use-package magit
  :defer 0

  :bind
  (("C-c g" . magit-status-with-node)
   ("C-c M-g" . magit-dispatch))

  :init
  (setq smerge-command-prefix (kbd "C-c m")))

(defun magit-status-with-node ()
  "Configure project's node_modules for Git hook commands to be working in Magit."
  (interactive)
  ;; `project-switch-project' only overrides `project-current-directory-override',
  ;; not `default-directory', so resolve the root via `project-current' to also
  ;; work when invoked from its dispatch menu (C-c p p)
  (let ((default-directory (if-let ((pr (project-current)))
                                (project-root pr)
                              default-directory)))
    (add-node-modules-path)
    (magit-status-setup-buffer)))


;; use diff-hl to add git diffs in gutter
(use-package diff-hl
  :defer 0
  :init
  (setq diff-hl-draw-borders nil)

  :config
  ;; needed for Magit 2.4 or newer
  ;; see: https://github.com/dgutov/diff-hl#magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode))

(provide 'initializer-vcs)
