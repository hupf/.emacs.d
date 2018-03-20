(use-package projectile
             :init
             ;; Have projectile persist its state into the data/ directory.
             (setq projectile-cache-file (f-join data-directory "projectile.cache"))
             (setq projectile-known-projects-file (f-join data-directory "projectile-bookmarks.eld"))
             :config (projectile-global-mode t))

;; TODO: remove?
;; go to project dir when selecting project
;; (custom-set-variables
;;  '(projectile-switch-project-action (quote projectile-dired)))

(provide 'initializer-projectile)
