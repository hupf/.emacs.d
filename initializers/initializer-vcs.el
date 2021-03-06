(use-package magit
  :defer 0

;; TODO: needed?
;; (global-magit-file-mode 1)

  :bind
  (("C-c g" . magit-status)
   ("C-c M-g" . magit-dispatch-popup)))

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
