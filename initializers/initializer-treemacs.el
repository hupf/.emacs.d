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
  (setq treemacs-show-hidden-files nil) ;; Hide dot files
  (setq treemacs-follow-after-init t) ;; Open folder & select file of selected buffer
  (setq treemacs-silent-refresh t) ;; No message after manual refresh
  (setq treemacs-silent-filewatch t) ;; No message after refresh in watch mode
  ;; (setq treemacs-no-png-images t) ;; Use text-only icons, no images
  (setq treemacs-indentation-string " ")

  ;; Font and colors
  (defvar treemacs-font-size (if (eq window-system 'x)
      (- default-font-size 1) ; Even a little bit smaller on Linux
    default-font-size))
  (defvar treemacs-font (concat treemacs-font-name " " (number-to-string treemacs-font-size)))
  (defvar treemacs-foreground "#9B9E94") ;; monokai-background with brightness 60
  ;(defvar treemacs-foreground "#B4B6AF") ;; monokai-background with brightness 70

  ;; Use custom-set-faces instead of use-package's :custom-face since
  ;; backquote expressions are working here
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
  ("C-c t" . treemacs)
  (:map treemacs-mode-map
    ;; Mouse click on file opens it in buffer
    ("<mouse-1>" . treemacs-single-click-expand-action)

    ;; Open with OS native browser/application
    ("C-<return>" . treemacs-natively-open-path-at-point))

  :config
  (treemacs-follow-mode -1) ;; Allow to scroll freely
  (treemacs-filewatch-mode)
  (treemacs-git-mode 'simple)

  ;; Theme/icons
  (treemacs-resize-icons 16)
  (treemacs-modify-theme "Default"
    :icon-directory (f-join user-emacs-directory "icons")
    :config
    (progn
      ;; These icons have been taken from:
      ;; https://github.com/microsoft/vscode-icons/tree/master/icons/dark
      ;; They have been cropped (to elminate margin), resized to 16x16 and color adjusted to #9B9E94
      (treemacs-create-icon :file "root-folder-lg.png" :extensions (root) :fallback "")
      (treemacs-create-icon :file "folder.png"    :extensions (dir-closed) :fallback (propertize "+ " 'face 'treemacs-term-node-face))
      (treemacs-create-icon :file "folder-opened.png" :extensions (dir-open) :fallback (propertize "- " 'face 'treemacs-term-node-face))
      (treemacs-create-icon :file "file.png" :extensions (fallback))
      (treemacs-create-icon :file "file-media.png" :extensions ("jpg" "jpeg" "bmp" "svg" "png" "xpm" "gif"))))
  )

(use-package treemacs-projectile
  :ensure t
  :defer t
)

(defun treemacs-natively-open-path-at-point ()
  "Open path at point with native browser/application."
  (interactive)
  (let ((command (if (eq system-type 'darwin) "open" "gio open"))
        (file (treemacs--prop-at-point :path)))
    (call-process-shell-command (concat command " " file) nil nil nil)))

(provide 'initializer-treemacs)
;;; initializer-treemacs.el ends here
