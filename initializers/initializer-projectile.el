;;; initializer-projectile.el --- Setup Projectile
;;;
;;; Commentary:
;;;
;;; Configuration of the Projectile project interaction library.
;;;
;;; Code:

(use-package projectile
  :diminish projectile-mode

  :custom
  ;; Selectrum bases on default completing-read completion mechanism
  (projectile-completion-system 'default)

  ;; (projectile-indexing-method 'git)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :bind
  ("C-S-t" . projectile-find-file)

  :config
  (projectile-mode t))

;; TODO: remove or open directory in treemacs?
;; go to project dir when selecting project
;; (custom-set-variables
;;  '(projectile-switch-project-action (quote projectile-dired)))

(provide 'initializer-projectile)
;;; initializer-projectile.el ends here
