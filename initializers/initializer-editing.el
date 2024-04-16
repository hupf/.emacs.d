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

;; Auto open/close pairs of parenthesises or quotes with built-in
;; electric-pair-mode
(use-package electric
  :diminish electric-pair-mode
  :config
  (electric-pair-mode t))

;; when you have a selection, typing text replaces it all.
(delete-selection-mode t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Scroll the display pixel-by-pixel, as opposed to only animating
;; line-by-line scrolls.
(pixel-scroll-precision-mode)

;; show matching paren
(use-package paren
  :defer 0
  :init (setq show-paren-style 'parenthesis)
  :config (show-paren-mode t))

;; highlight current line
(global-hl-line-mode t)

;; visual feedback to some operations by highlighting portions
;; relating to the operations.
(use-package volatile-highlights
  :defer 0
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; Visual undo tree
(use-package undo-tree
  :defer 0
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
  :bind ("M-y" . browse-kill-ring))

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click))

;; make the left fringe 2 pixels so the hl-diff indicators aren't so fat
;; leave the right fringe width at the default 8 pixels
(fringe-mode '(2 . 8))

(use-package flycheck
  ;; Don't diminish to make flycheck-status-emoji-mode working
  ;; :diminish flycheck-mode

  :custom
  ;; (flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  ;; Setup flycheck to show on the right side of the buffer
  (flycheck-indication-mode 'right-fringe)

  :hook (prog-mode . flycheck-mode)

  :config
  ;; Make the flycheck arrow look like an exclamation point, but only
  ;; do it when emacs runs in a window, not terminal
  (when window-system
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [0 24 24 24 24 24 24 0 0 24 24 0 0 0 0 0 0])))

;; Display emoji with the flycheck status (replaces default flycheck modeline)
(use-package flycheck-status-emoji
  :hook (flycheck-mode . flycheck-status-emoji-mode))

;; Git Gutter Use git-gutter-fringe if not in TTY, since it is
;; compatible with nlinum
(use-package git-gutter
  :defer 0
  :diminish git-gutter-mode

  :init
  (setq git-gutter-fr:side 'right-fringe)

  :config
  (when (display-graphic-p)
    (use-package git-gutter-fringe))
  (global-git-gutter-mode)

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
      )))


;; Show whitespace
(use-package whitespace
  :diminish whitespace-mode

  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)

  :config
  (setq whitespace-style '(face tabs empty trailing lines-tail)
        whitespace-line-column 80
        whitespace-line nil)) ;; Don't style long lines

;; Highlight tabs with same color as trailing whitespaces
(add-hook 'font-lock-mode-hook (lambda ()
  (font-lock-add-keywords nil
    '(("\t" 0 'trailing-whitespace prepend)))))

;; Use faster, C-implemented tree-sitter instead of regex-based
;; font-lock-mode for syntax highlighting
(use-package tree-sitter
  ;; :init
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))

  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)

  :hook
  (typescript-mode . tree-sitter-hl-mode)
  ;; TODO: enable generically
  ;; (prog-mode . setup-tree-sitter-highlighting)

  :diminish
  tree-sitter-hl-mode)

;; TODO: how to avoid the void variable error?
;; (defun setup-tree-sitter-highlighting ()
;;   "Enable syntax highlighting with tree-sitter if current major-mode is supported."
;;   (message "setup-tree-sitter-highlighting %s" (member major-mode (mapcar #'car (or tree-sitter-major-mode-language-alist '()))))
;;   (when (member major-mode (mapcar #'car (or tree-sitter-major-mode-language-alist '()))) (tree-sitter-hl-mode) (message "tree-sitter highlighting enabled")))

;; (use-package tree-sitter-langs
;;   :after tree-sitter)

;; Emacs creates lockfiles to recognize when someone else is already
;; editing the same file as you. Many JavaScript development servers
;; cannot handle these files in watch-mode so disable the lockfiles
;; feature
(setq create-lockfiles nil)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; disable the blinking cursor
(blink-cursor-mode nil)

;; Drag stuff around.
(use-package drag-stuff
  :diminish drag-stuff-mode

  :bind
  ("M-S-<up>" . drag-stuff-up)
  ("M-S-<down>" . drag-stuff-down)

  :config
  (drag-stuff-global-mode))


(use-package cycle-quotes
  :commands
  (cycle-quotes)

  :bind
  ("C-S-q" . cycle-quotes))


;; Increase selected region by semantic units
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))


;; displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config (which-key-mode t))


;; Yet another snippet library, which is awesome. Allows you to expand
;; commonly used code templates into your buffer. Use it everywhere!
;; see https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :defer 0
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
  :defer 0
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))


;; Prettier
(use-package prettier-js
  :diminish prettier-js-mode

  :hook (((js-mode typescript-mode json-mode web-mode css-mode scss-mode markdown-mode yaml-mode) . init-prettier)))

(defun init-prettier ()
  "Initialize Prettier mode, making sure the project-local version is used."

  ;; Don't install Prettier globally. Install a pinned Prettier
  ;; version in the projects you'd like formatting to be enabled.

  ;; Activate the project's Node version (you may want to add a .npmrc
  ;; file with the desired Node version to your project directory)
  (nvm-use-for-buffer)

  ;; Make sure the project-local ./node_modules/.bin/prettier is found
  (add-node-modules-path)

  ;; Enable formatting on save
  (prettier-js-mode))

;; enable y/n answers so you don't have to type 'yes' on 'no'
;; for everything
(fset 'yes-or-no-p 'y-or-n-p)

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
  "Delete (not kill) word forward."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(global-set-key (kbd "C-<delete>") 'delete-word)
(global-set-key (kbd "C-<kp-delete>") 'delete-word)

(defun backward-delete-word (arg)
  "Delete (not kill) word backward."
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


;; ripgrep search (install `cargo install ripgrep`)
(use-package rg
  :commands (rg rg-literal rg-project)

  :init
  (setq rg-executable (concat (getenv "HOME") "/.cargo/bin/rg")
        rg-show-header nil)

  :bind ("C-S-f" . rg))

;; SVGO
(use-package svgo
  :hook
  (nxml-mode . (lambda () (bind-key "M-o" 'svgo nxml-mode-map)))
  (image-mode . (lambda () (bind-key "M-o" 'svgo image-mode-map))))

(provide 'initializer-editing)

;;; initializer-editing.el ends here
