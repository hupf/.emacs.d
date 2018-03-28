(use-package magit
  :ensure t)
;; TODO: needed?
;; (global-magit-file-mode 1)

;; use diff-hl to add git difs in gutter
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

;; TODO
;; ;; Open github links in the browser
;; (setq git-link-open-in-browser t)

(provide 'initializer-vcs)
