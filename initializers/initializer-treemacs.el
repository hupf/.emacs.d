;;; initializer-treemacs.el --- Setup Treemacs
;;;
;;; Commentary:
;;;
;;; Configuration of the Treemacs project explorer.
;;;
;;; Code:

(use-package treemacs
  :custom
  (treemacs-read-string-input 'from-minibuffer) ;; Fix cursor problem with child frame pop up by using traditional minibuffer input (see https://github.com/Alexander-Miller/treemacs/issues/763)
  (treemacs-show-hidden-files nil) ;; Hide dot files
  (treemacs-follow-after-init t) ;; Open folder & select file of selected buffer
  (treemacs-silent-refresh t) ;; No message after manual refresh
  (treemacs-silent-filewatch t) ;; No message after refresh in watch mode
  (treemacs-indentation-string " ")
  ;;(treemacs-no-png-images t) ;; Use text-only icons, no images

  :init
  ;; Font and colors
  (let* ((treemacs-font (concat treemacs-font-name " " (number-to-string treemacs-font-size)))
         ;;(treemacs-foreground "#B4B6AF") ;; monokai-background with brightness 70
         (treemacs-foreground "#9B9E94")) ;; monokai-background with brightness 60

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
     `(treemacs-tags-face ((t (:inherit default :font ,treemacs-font :foreground ,monokai-violet))))))

  :bind
  ("C-c t" . treemacs)
  (:map treemacs-mode-map
    ;; Single click on directory should expand it, on file should open it
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
      (treemacs-create-icon :file "root-folder.png" :extensions (root) :fallback "")
      (treemacs-create-icon :file "root-folder.png" :extensions (root-closed) :fallback "")
      (treemacs-create-icon :file "root-folder-opened.png" :extensions (root-open) :fallback "")
      (treemacs-create-icon :file "folder.png"    :extensions (dir-closed) :fallback (propertize "+ " 'face 'treemacs-term-node-face))
      (treemacs-create-icon :file "folder-opened.png" :extensions (dir-open) :fallback (propertize "- " 'face 'treemacs-term-node-face))
      (treemacs-create-icon :file "file.png" :extensions (fallback))
      (treemacs-create-icon :file "file-media.png" :extensions ("jpg" "jpeg" "bmp" "svg" "png" "xpm" "gif"))))

  :hook
  (treemacs-mode . treemacs-fix-single-click-root-expand))

(use-package treemacs-projectile
  :after treemacs)

(defun treemacs-natively-open-path-at-point ()
  "Open path at point with native browser/application."
  (interactive)
  (let ((command (if (eq system-type 'darwin) "open" "gio open"))
        (file (treemacs--prop-at-point :path)))
    (call-process-shell-command (concat command " " file) nil nil nil)))

(defun treemacs-fix-single-click-root-expand ()
  "Avoid a Mouse-2 action being performed when only clicking shortly on root nodes."
  (set (make-local-variable 'mouse-1-click-follows-link) nil))

(provide 'initializer-treemacs)
;;; initializer-treemacs.el ends here
