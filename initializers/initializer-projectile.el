;;; initializer-projectile.el --- Setup Projectile
;;;
;;; Commentary:
;;;
;;; Configuration of the Projectile project interaction library.
;;;
;;; Code:

(use-package projectile
  :diminish projectile-mode

  :init
  ;; (setq projectile-indexing-method 'git)

  ;; Selectrum bases on default completing-read completion mechanism
  (setq projectile-completion-system 'default)

  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

;; TODO: remove?
;; go to project dir when selecting project
;; (custom-set-variables
;;  '(projectile-switch-project-action (quote projectile-dired)))

(provide 'initializer-projectile)
;;; initializer-projectile.el ends here
