(use-package magit
  :ensure t)
;; TODO: needed?
;; (global-magit-file-mode 1)

;; use diff-hl to add git diffs in gutter
(use-package diff-hl
  :ensure t

  :init
  (setq diff-hl-draw-borders nil)

  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup))

  :config
  ;; needed for Magit 2.4 or newer
  ;; see: https://github.com/dgutov/diff-hl#magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode))

(provide 'initializer-vcs)
