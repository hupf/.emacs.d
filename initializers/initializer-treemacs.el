;;; initializer-treemacs.el --- Setup Treemacs
;;;
;;; Commentary:
;;;
;;; Configuration of the Treemacs project explorer.
;;;
;;; Code:

(use-package treemacs
  :ensure t
  :defer t

  :init
  (setq treemacs-persist-file (f-join user-data-directory "treemacs.cache"))
  (setq treemacs-show-hidden-files nil) ;; Hide dot files
  (setq treemacs-follow-after-init t) ;; Open folder & select file of selected buffer
  (setq treemacs-silent-refresh t) ;; No message after manual refresh
  (setq treemacs-silent-filewatch t) ;; No message after refresh in watch mode
  ;; (setq treemacs-no-png-images t) ;; Use text-only icons, no images
  (setq treemacs-indentation-string " ")

  (treemacs-resize-icons 16)

  ;; Use unicode characters as directory & file icons
  ;; TODO: slows down treemacs toggle and displays no icons on mac
  (if (not (eq system-type 'darwin))
    (setq treemacs-icon-open-text (propertize "📂 " 'face 'treemacs-directory-face)
          treemacs-icon-closed-text (propertize "📁 " 'face 'treemacs-directory-face)
          treemacs-icon-fallback-text (propertize "" 'face 'treemacs-file-face))
  )

  ;; Redefine the following constant after treemacs-persistence.el has
  ;; been evaluated
  (with-eval-after-load "treemacs-persistence"
    (setq treemacs--last-error-persist-file (f-join user-data-directory "treemacs.last-error")))

  ;; Use custom-set-faces instead of use-package's :custom-face since
  ;; backquote expressions are working here
  (defvar treemacs-font "IBM Plex Sans Light 14")
  (defvar treemacs-foreground "#9B9E94") ;; monokai-background with brightness 60
  ;(defvar treemacs-foreground "#B4B6AF") ;; monokai-background with brightness 70
  (custom-set-faces
   `(treemacs-root-face ((t (:inherit default :font ,treemacs-font :height 1.2 :weight demibold :foreground ,treemacs-foreground))))
   `(treemacs-file-face ((t (:inherit default :font ,treemacs-font :foreground ,treemacs-foreground))))
   `(treemacs-directory-face ((t (:inherit default :font ,treemacs-font :foreground ,treemacs-foreground))))
   `(treemacs-git-modified-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-yellow))))
   `(treemacs-git-renamed-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-cyan))))
   `(treemacs-git-ignored-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-gray))))
   `(treemacs-git-untracked-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-orange))))
   `(treemacs-git-added-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-green))))
   `(treemacs-git-conflict-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-red))))
   `(treemacs-tags-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-violet)))))

  :bind
  ("C-S-b" . treemacs)
  (:map treemacs-mode-map
    ;; Mouse click on file opens it
    ("<mouse-1>" . treemacs-single-click-expand-action)

    ;; Open with OS native browser/application
    ("C-<return>" . treemacs-natively-open-path-at-point))

  :config
  (treemacs-follow-mode -1) ;; Allow to scroll freely
  (treemacs-filewatch-mode)
  (treemacs-git-mode 'simple)
  )

;; Execute M-x all-the-icons-install-fonts to download the fonts
;; (use-package treemacs-all-the-icons
;;   :ensure t
;;   :after treemacs
;;   :config (treemacs-load-theme 'all-the-icons))

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
;;; initializer-treemacs.el ends here
