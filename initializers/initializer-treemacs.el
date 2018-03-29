;; (use-package treemacs
;;   :ensure t

;;   :init
;;   (setq treemacs-show-hidden-files nil) ; hide dot files
;;   (setq treemacs-follow-after-init t) ; open folder & select file of selected buffer
;;   (setq treemacs-silent-refresh t) ; no message after manual refresh
;;   (setq treemacs-silent-filewatch t) ; no message after refresh in watch mode
;;   (setq treemacs-no-png-images t) ; use text-only icons, no images

;;   :bind
  

;;   :config

;;   )




;; (defun treemacs-natively-open-path-at-point ()
;;   "Open path at point with native browser/application"
;;   (interactive)
;;   (let ((command (if (eq system-type 'darwin) "open" "gnome-open")))
;;        (call-process-shell-command command nil nil nil (treemacs--prop-at-point 'abs-path))))

;; (add-hook 'treemacs-mode-hook
;;           '(lambda ()
;;              ; fix mouse click to open file
;;              (local-set-key (kbd "<mouse-1>") 'treemacs-visit-node-default-action)

;;              (local-set-key (kbd "C-<return>") 'treemacs-natively-open-path-at-point)

;;              (treemacs-filewatch-mode)
;;              (treemacs-git-mode 'simple)))

;; ; activate treemacs-projectile if switching to project and treemacs is open
;; (add-hook 'projectile-after-switch-project-hook
;;           '(lambda ()
;;              (if (eq (treemacs--current-visibility) 'visible)
;;                  (treemacs-projectile))))

;; ; keybinding to toggle treemacs(-projectile) depending on whether within projectile project
;; (global-set-key (kbd "C-S-b")
;;                 '(lambda ()
;;                    (interactive)
;;                    (if (eq (projectile-project-p) nil) (treemacs-toggle) (treemacs-projectile-toggle))))

;; ; icons
;; (with-eval-after-load "treemacs"
;;   (setq treemacs-icon-open-text (propertize "📂 " 'face 'treemacs-directory-face)
;;         treemacs-icon-closed-text (propertize "📁 " 'face 'treemacs-directory-face)
;;         treemacs-icon-fallback-text (propertize "  " 'face 'treemacs-file-face)))

;; ; fonts
;; (custom-set-faces
;;  '(treemacs-directory-face ((t (:inherit font-lock-type-face :height 90))))
;;  '(treemacs-header-face ((t (:inherit font-lock-comment-face))))
;;  '(treemacs-file-face ((t (:inherit default :height 90))))
;;  '(treemacs-term-node-face ((t (:inherit font-lock-string-face :height 90))))
;;  '(treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face :height 90))))
;;  '(treemacs-git-renamed-face ((t (:inherit font-lock-doc-face :height 90))))
;;  '(treemacs-git-ignored-face ((t (:inherit font-lock-comment-face :height 90))))
;;  '(treemacs-git-untracked-face ((t (:inherit font-lock-string-face :height 90))))
;;  '(treemacs-git-added-face ((t (:inherit font-lock-type-face :height 90))))
;;  '(treemacs-git-conflict-face ((t (:inherit error :height 90))))
;;  '(treemacs-tags-face ((t (:inherit font-lock-builtin-fac :height 90))))
;; )

;; TODOS
;; weitere fonts customizen (git etc.)
;; files icon space
;; file/directory moven umständlich über rename (keine verzeichnis completion, filename muss angegeben werden)
;; directories öffnen schliessen mit < > pfeilen
