(use-package projectile
  :ensure t

  :diminish projectile-mode

  :init
  ;; Have projectile persist its state into the data/ directory.
  (setq projectile-cache-file (f-join user-data-directory "projectile.cache"))
  (setq projectile-known-projects-file (f-join user-data-directory "projectile-bookmarks.eld"))
  ;; (setq projectile-indexing-method 'git)

  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; TODO: remove?
;; go to project dir when selecting project
;; (custom-set-variables
;;  '(projectile-switch-project-action (quote projectile-dired)))

(provide 'initializer-projectile)
