;;; initializer-editing.el --- What happens when I edit things?
;;;
;;; Commentary:
;;;
;;; Behaviors for when you edit things.
;;;
;;; Code:

;; Death to the tabs indeed!
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el#L35-L44
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance


;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; always end files with newlines
(setq require-final-newline t)

;; setup smartparens to auto open and close pairs
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config (smartparens-global-mode 1))

;; when you have a selection, typing text replaces it all.
(delete-selection-mode t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; show matching paren
(use-package paren
  :ensure t
  :init (setq show-paren-style 'parenthesis)
  :config (show-paren-mode t))

;; highlight current line
(global-hl-line-mode t)

;; visual feedback to some operations by highlighting portions
;; relating to the operations.
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; Visual undo tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode

  :config (global-undo-tree-mode t)

  :bind
  (:map undo-tree-map
        ("C-/" . nil))) ;; Remove mapping

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Choose and yank one of the last killed texts
(use-package browse-kill-ring
  :ensure t
  :bind ("M-y" . browse-kill-ring))

(use-package multiple-cursors
  :ensure t
  :bind ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)
        )

;; make the left fringe 2 pixels so the hl-diff indicators aren't so fat
;; leave the right fringe width at the default 8 pixels
(fringe-mode '(2 . 8))

(use-package flycheck
  :ensure t

  :diminish flycheck-mode

  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  ;; Setup flycheck to show on the right side of the buffer
  (setq flycheck-indication-mode 'right-fringe)

  :hook (prog-mode . flycheck-mode)

  :config
  ;; Make the flycheck arrow look like an exclamation point, but only
  ;; do it when emacs runs in a window, not terminal
  (when window-system
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [0 24 24 24 24 24 24 0 0 24 24 0 0 0 0 0 0]))
  )

;; Git Gutter Use git-gutter-fringe if not in TTY, since it is
;; compatible with nlinum
(use-package git-gutter
  :ensure t

  :diminish
  git-gutter-mode

  :config
  (setq git-gutter-fr:side 'right-fringe)
  (when (display-graphic-p)
    (fringe-helper-define 'git-gutter-fr:added nil
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXX.XXX"
      "XXX.XXX"
      "X.....X"
      "XXX.XXX"
      "XXX.XXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      )
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "X.....X"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      )
    (fringe-helper-define 'git-gutter-fr:modified nil
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "X.....X"
      "XXXXXXX"
      "X.....X"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      "XXXXXXX"
      ))
  :init
  (when (display-graphic-p)
    (use-package git-gutter-fringe
      :ensure t))
  (global-git-gutter-mode))


;; Show whitespace
(use-package whitespace
:ensure t
:diminish whitespace-mode

:init
(dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
(add-hook 'before-save-hook #'whitespace-cleanup)

:config
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))
)

;; Highlight tabs with same color as trailing whitespaces
(add-hook 'font-lock-mode-hook (lambda ()
  (font-lock-add-keywords nil
    '(("\t" 0 'trailing-whitespace prepend)))))


;; backup and autosave files go into the tmp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; disable the blinking cursor
(blink-cursor-mode nil)

;; Drag stuff around.
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode

  :bind
  ("M-S-<up>" . drag-stuff-up)
  ("M-S-<down>" . drag-stuff-down)

  :config
  (drag-stuff-global-mode))


;; Increase selected region by semantic units
(use-package expand-region
  :ensure t
  :commands er/expand-region

  :bind
  ("C-=" . er/expand-region))


;; displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode t))


;; Yet another snippet library, which is awesome. Allows you to expand
;; commonly used code templates into your buffer. Use it everywhere!
;; see https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :ensure t

  :diminish yas-minor-mode

  :config
  (setq yas-verbosity 1) ; No need to be so verbose
  (setq yas-wrap-around-region t)
  (use-package yasnippet-snippets
:ensure t)
  (yas-global-mode t))


;; Editorconfig
;; (make sure to `apt install editorconfig` or `brew install editorconfig`)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))


;; Prettier

(use-package prettier-js
  :ensure t
  :diminish prettier-js-mode

  :hook ((js2-mode . init-prettier)
         (typescript-mode . init-prettier)
         (json-mode . init-prettier))
  )

(defun init-prettier ()
  ;; Use project-local prettier command -- make sure, prettier is not
  ;; installed globally, so it is only active in projects with
  ;; prettier installed.
  (add-node-modules-path)
  (prettier-js-mode)
  )

;; ;; Emacs creates lockfiles to recognize when someone else is already
;; ;; editing the same file as you.
;; ;;
;; ;; Ember-CLI doesn't know what to do with these lock files. One second
;; ;; they are there and the next the lock file disappears. This causes
;; ;; issues with Ember-CLI's livereload feature where you will commonly
;; ;; get an error like:
;; ;;
;; ;; Error: ENOENT, no such file or directory '.../components/.#file-name.hbs'
;; ;;
;; ;; To solve this issue we set "create-lockfiles" to nil and it will no
;; ;; longer create these lock files.
;; (setq create-lockfiles nil)

;; enable y/n answers so you don't have to type 'yes' on 'no'
;; for everything
(fset 'yes-or-no-p 'y-or-n-p)

;; ;; Autosave when switching buffers, windows, or frames.
;; ;; Note: Emacs has different concepts of buffers, windows and frames
;; ;; than you might be used to.
;; ;;
;; ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffers-and-Windows.html
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Frames.html
;; (defadvice switch-to-buffer (before save-buffer-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice other-window (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-up (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-down (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-left (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-right (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))

;; (add-hook 'focus-out-hook (lambda () (when buffer-file-name (save-buffer))))

;; ;; This makes indenting region and untabifying region work on the entire
;; ;; buffer if no region is selected
;; ;; https://github.com/bbatsov/crux#using-the-bundled-advices
;; (require 'crux)
;; (crux-with-region-or-buffer indent-region)
;; (crux-with-region-or-buffer untabify)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

(defun delete-word (arg)
  "Delete (not kill) word forward"
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(global-set-key (kbd "C-<delete>") 'delete-word)
(global-set-key (kbd "C-<kp-delete>") 'delete-word)

(defun backward-delete-word (arg)
  "Delete (not kill) word backward"
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

(defun disable-final-newline ()
  "Disable adding of final newline when saving current buffer."
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(defun dos2unix ()
  "Convert DOS CRs to Unix LFs."
  (interactive)
  (set-buffer-file-coding-system 'unix 't))


;; rgrep
(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))


(provide 'initializer-editing)

;;; initializer-editing.el ends here
