(use-package treemacs
  :ensure t
  :defer t

  :init
  (setq treemacs-show-hidden-files nil) ;; Hide dot files
  (setq treemacs-follow-after-init t) ;; Open folder & select file of selected buffer
  (setq treemacs-silent-refresh t) ;; No message after manual refresh
  (setq treemacs-silent-filewatch t) ;; No message after refresh in watch mode
  (setq treemacs-no-png-images t) ;; Use text-only icons, no images

  ;; Use unicode characters as directory & file icons
  ;; TODO: slows down treemacs toggle and displays no icons on mac
  (if (not (eq system-type 'darwin))
    (setq treemacs-icon-open-text (propertize "📂 " 'face 'treemacs-directory-face)
          treemacs-icon-closed-text (propertize "📁 " 'face 'treemacs-directory-face)
          treemacs-icon-fallback-text (propertize "  " 'face 'treemacs-file-face))
  )

  :custom-face
  (treemacs-directory-face ((t (:inherit font-lock-type-face :height 90))))
  (treemacs-header-face ((t (:inherit font-lock-comment-face))))
  (treemacs-file-face ((t (:inherit default :height 90))))
  (treemacs-term-node-face ((t (:inherit font-lock-string-face :height 90))))
  (treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face :height 90))))
  (treemacs-git-renamed-face ((t (:inherit font-lock-doc-face :height 90))))
  (treemacs-git-ignored-face ((t (:inherit font-lock-comment-face :height 90))))
  (treemacs-git-untracked-face ((t (:inherit font-lock-string-face :height 90))))
  (treemacs-git-added-face ((t (:inherit font-lock-type-face :height 90))))
  (treemacs-git-conflict-face ((t (:inherit error :height 90))))
  (treemacs-tags-face ((t (:inherit font-lock-builtin-fac :height 90))))

  :bind
  ("C-S-b" . treemacs-toggle-auto-projectile)
  (:map treemacs-mode-map
    ;; Mouse click on file opens it
    ;; ("<mouse-1>" . treemacs-visit-node-no-split)

    ;; Open with OS native browser/application
    ("C-<return>" . treemacs-natively-open-path-at-point))

  :config
  (treemacs-filewatch-mode)
  (treemacs-git-mode 'simple)

  ;; Activate treemacs-projectile if switching to project and treemacs is open
  (add-hook 'projectile-after-switch-project-hook
    '(lambda ()
       (if (eq (treemacs--current-visibility) 'visible)
           (treemacs-projectile))))
)

(use-package treemacs-projectile
  :ensure t
  :defer t
)

(defun treemacs-toggle-auto-projectile ()
  "Toggle treemacs considering whether in projectile project"
  (interactive)
  (if (eq (projectile-project-p) nil) (treemacs-toggle) (treemacs-projectile-toggle)))

(defun treemacs-natively-open-path-at-point ()
  "Open path at point with native browser/application"
  (interactive)
  ;; TODO: fix, treemacs--prop-at-point returns nil
  (let ((command (if (eq system-type 'darwin) "open" "gnome-open")))
       (call-process-shell-command command nil nil nil (treemacs--prop-at-point 'abs-path))))

(provide 'initializer-treemacs)
