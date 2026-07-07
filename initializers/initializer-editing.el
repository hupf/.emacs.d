;;; initializer-editing.el --- What happens when I edit things? -*- lexical-binding: t; -*-
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
(make-variable-buffer-local 'global-hl-line-mode) ;; Allow to locally disable using (setq global-hl-line-mode nil)

;; visual feedback to some operations by highlighting portions
;; relating to the operations.
(use-package volatile-highlights
  :defer 0
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; Visual undo tree (on-demand visualization, works on top of built-in
;; undo)
(use-package vundo
  :bind ("C-c u" . vundo))

;; Keep region when undoing in region
(defun undo--keep-region (orig-fn &rest args)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        (apply orig-fn args)
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    (apply orig-fn args)))
(advice-add 'undo :around #'undo--keep-region)

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

;; Tree-sitter grammars for the `*-ts-mode' major modes configured
;; throughout this config. Run `M-x my/treesit-install-all-grammars'
;; once (and again after Emacs upgrades) to (re)compile them.
(setq treesit-language-source-alist
      '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.23.11" "php/src"))
        (phpdoc . ("https://github.com/claytonrcarter/tree-sitter-phpdoc"))))

(defun my/treesit-install-all-grammars ()
  "Install every grammar listed in `treesit-language-source-alist'."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (treesit-install-language-grammar lang)))

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

  :hook (((js-ts-mode typescript-ts-mode tsx-ts-mode json-ts-mode web-mode css-ts-mode scss-mode markdown-mode yaml-ts-mode) . init-prettier)))

(defun init-prettier ()
  "Initialize Prettier mode, making sure the project-local version is used."

  ;; Don't install Prettier globally, install a Prettier version in
  ;; the projects you'd like formatting to be enabled.

  ;; Make sure the project-local ./node_modules/.bin/prettier is found
  (add-node-modules-path)

  ;; Enable formatting on save
  (prettier-js-mode))

;; enable y/n answers so you don't have to type 'yes' on 'no'
;; for everything
(setq use-short-answers t)

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


;; ripgrep search (install `cargo install ripgrep`)
(use-package rg
  :commands (rg rg-literal rg-project)

  :init
  (setq rg-show-header nil)

  :bind ("C-S-f" . rg))

;; SVGO
(use-package svgo
  :hook
  (nxml-mode . (lambda () (bind-key "M-o" 'svgo nxml-mode-map)))
  (image-mode . (lambda () (bind-key "M-o" 'svgo image-mode-map))))

(provide 'initializer-editing)

;;; initializer-editing.el ends here
