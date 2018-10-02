(use-package treemacs
  :ensure t
  :defer t

  :init
  (setq treemacs-persist-file (f-join user-data-directory "treemacs.cache"))
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
  ;; TODO: bigger font for mac, smaller for linux?
  ;; (treemacs-directory-face ((t (:inherit font-lock-type-face :height 90))))
  ;; (treemacs-header-face ((t (:inherit font-lock-comment-face))))
  ;; (treemacs-file-face ((t (:inherit default :height 90))))
  ;; (treemacs-term-node-face ((t (:inherit font-lock-string-face :height 90))))
  ;; (treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face :height 90))))
  ;; (treemacs-git-renamed-face ((t (:inherit font-lock-doc-face :height 90))))
  ;; (treemacs-git-ignored-face ((t (:inherit font-lock-comment-face :height 90))))
  ;; (treemacs-git-untracked-face ((t (:inherit font-lock-string-face :height 90))))
  ;; (treemacs-git-added-face ((t (:inherit font-lock-type-face :height 90))))
  ;; (treemacs-git-conflict-face ((t (:inherit error :height 90))))
  ;; (treemacs-tags-face ((t (:inherit font-lock-builtin-fac :height 90))))

  :bind
  ("C-S-b" . treemacs)
  (:map treemacs-mode-map
    ;; Mouse click on file opens it
    ("<mouse-1>" . treemacs-single-click-expand-action)

    ;; Open with OS native browser/application
    ("C-<return>" . treemacs-natively-open-path-at-point))

  :config
  (treemacs-filewatch-mode)
  (treemacs-git-mode 'simple)
)

(use-package treemacs-projectile
  :ensure t
  :defer t
)

(defun treemacs-natively-open-path-at-point ()
  "Open path at point with native browser/application"
  (interactive)
  ;; TODO: use "gio open" instead of "gnome-open"
  (let ((command (if (eq system-type 'darwin) "open" "gnome-open")))
       (call-process-shell-command command nil nil nil (treemacs--prop-at-point :path))))

(provide 'initializer-treemacs)
