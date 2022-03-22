(use-package magit
  :defer 0

  :bind
  (("C-c g" . magit-status-with-node)
   ("C-c M-g" . magit-dispatch)))

(defun magit-status-with-node ()
  "Configure project's Node.js and node_modules for Git hook commands to be working in Magit"
  (interactive)
  (nvm-use-for (magit-toplevel))
  (add-node-modules-path)
  (magit-status-setup-buffer))


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
