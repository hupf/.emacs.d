;; GENERAL
(setq frame-title-format "%b - Emacs") ;; filename in title bar

(if (functionp 'tool-bar-mode) (tool-bar-mode -1)) ;; no toolbar
(if (functionp 'menu-bar-mode) (menu-bar-mode -1)) ;; no menubar

(setq case-fold-search t) ;; search insensitive

(setq visible-bell 'top-bottom) ;; only visible bell
;(setq ring-bell-function 'ignore) ;; switch bell off completely

;(setq make-backup-files 0) ;; disable backup~ files
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t ; use versioned backups
)

;(global-linum-mode 1) ; display line numbers in all files

;; show columns and line numbers at the bottom
;(column-number-mode 1)

;(global-visual-line-mode t) ; proper word wrapping

;(setq debug-on-error t) ; enable for debugging

; define code directory
;; (setq code-directory "~/projects")
;; (when (eq system-type 'darwin)
;;   (setq code-directory "~/Development")
;; )


;; SCROLLING
(when (eq window-system 'x)
    (set-scroll-bar-mode 'right) ;; scrollbar right
)

;; enable smooth scrolling with mouse wheel
;; TODO scroll buffer where mouse pointer is
;; (defun smooth-scroll (number-lines increment)
;;   (if (= 0 number-lines)
;;       t
;;     (progn
;;       (sit-for 0.02)
;;       (scroll-up increment)
;;       (smooth-scroll (- number-lines 1) increment))))
;; (global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 5 1)))
;; (global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 5 -1)))


;; Enable Copy & Paste to/from Emacs in X11
(when (eq window-system 'x)
    (setq x-select-enable-clipboard t)
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
)


;; ENCODINGS
(setq current-language-environment "UTF-8")
(setq default-input-method "UTF-8")
(when (eq system-type 'gnu/linux)
    (require 'iso-transl) ;; see http://www.emacswiki.org/emacs/DeadKeys
)
(when (eq system-type 'mac)
    (set-keyboard-coding-system 'mac-roman)
)


;; FUNCTIONS
(defun disable-final-newline ()
  "Allow to disable adding of final newline, in a certain buffer."
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(defun dos2unix ()
  "Convert DOS carriage returns to Unix linefeeds"
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
      "python -mjson.tool" (current-buffer) t)))

(defun linum-off ()
  "Allow to disable 'linum-mode', once enabled in a buffer or globally"
  (interactive)
  (global-linum-mode 0)
  (linum-mode 0))

(defun get-project-root (buf repo-type)
  (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))
(defun current-buffer-project-root ()
  (get-project-root (current-buffer ".git")))


;; PACKAGES
(setq package-list '(

    ;; editor features
    auto-complete
    autopair
    editorconfig ;; dependency: apt-get install editorconfig
    dired-details+
    sr-speedbar
    ;; tabbar
    git-timemachine
    git-gutter
    grizzl ; for import-js

    ;; languages
    php-mode
    sass-mode
    scss-mode
    haml-mode
    json-mode

    ;; javascript
    js2-mode
    coffee-mode
    ac-js2
    import-js
    nvm
    add-node-modules-path

    ;; ruby
    ruby-mode
    enh-ruby-mode
    rvm
    robe

    ;; syntax checking/linting
    flycheck
    rubocop

))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; require packages needed for configuration
(require 'dired)
(require 'auto-complete-config)
(require 'sr-speedbar)

;; MODE OPTIONS

;; dired
(add-hook 'dired-mode-hook 'auto-revert-mode)
(put 'dired-find-alternate-file 'disabled nil)

;(setq-default dired-omit-mode t)
;(setq-default dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
;(add-to-list 'dired-omit-extension ".example")
;(delete 'dired-omit-extension ".example")

;(add-to-list 'load-path (concat code-directory "/dired-single"))
;(require 'dired-single)
;(setq dired-single-open-files-in-other-window 1)

;(define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
;(define-key dired-mode-map [down-mouse-1] 'dired-single-buffer-mouse)
;(define-key dired-mode-map "."
;  (function
;   (lambda nil (interactive) (dired-single-buffer ".."))))

;; tabbar
;; (add-to-list 'load-path (concat code-directory "/tabbar"))
;; (require 'tabbar)
;; (setq tabbar-buffer-groups-function
;;       (lambda ()
;;         (list "All")))
;; (setq tabbar-buffer-list-function
;;       (lambda ()
;;         (remove-if
;;          (lambda(buffer)
;;            (find (aref (buffer-name buffer) 0) " *"))
;;          (buffer-list))))
;; (setq 'tabbar-use-images nil)


;; flycheck
(add-hook 'ruby-mode-hook
          '(lambda ()
             ; disable other ruby checkers since only setting flycheck-checker
             ; somehow doesn work
             (setq flycheck-disabled-checkers '(ruby-rubylint ruby ruby-jruby))

             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)))


;; autocomplete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)


;;autopair
(autopair-global-mode)
(add-hook 'minibuffer-setup-hook
          (lambda () (autopair-mode -1)))


;; git-gutter
(global-git-gutter-mode 1)


;; nvm
(require 'nvm)
(nvm-use-for) ; activate node version specified in ~/.nvmrc



;; js
;; (add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook
          '(lambda ()
             (ac-js2-mode)
             (nvm-use-for) ;; use project's node version
             (add-node-modules-path) ;; allow to access project's eslint

             ;; To workaround messages outputted by eslint on STDERR,
             ;; use a wrapper script that ignore STDERR
             (make-local-variable 'exec-path)
             (add-to-list 'exec-path "~/bin")

             ;; Put this script into ~/bin/eslint:
             ;; #!/usr/bin/env bash
             ;; PROJECT_ROOT=$(pwd -P 2>/dev/null || command pwd)
             ;; while [ ! -e "$PROJECT_ROOT/node_modules" ]; do
             ;; PROJECT_ROOT=${PROJECT_ROOT%/*}
             ;; if [ "$PROJECT_ROOT" = "" ]; then break; fi
             ;; done
             ;; ESLINT=$PROJECT_ROOT/node_modules/.bin/eslint
             ;; $ESLINT "$@" 2> /dev/null

             ;; use eslint, not jshint checker
             (setq flycheck-disabled-checkers '(javascript-jshint))
             (flycheck-select-checker 'javascript-eslint)
             (flycheck-mode 1)))


(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-highlight-level 3)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override t)
(setq js2-indent-switch-body t)
(setq js-switch-indent-offset 2)


;; coffee
(setq coffee-tab-width 2)
(setq coffee-indent-tabs-mode nil)



;; (enh-)ruby-mode
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))
(add-hook 'enh-ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))
(setq ruby-insert-encoding-magic-comment nil)


;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))


;; MODE ASSOCIATIONS
(setq default-major-mode 'text-mode) ;; default mode is text-mode

;; correct mode for .emacs file
(add-to-list 'auto-mode-alist '("\\.emacs$" . lisp-mode))
(add-to-list 'auto-mode-alist '("dotemacs$" . lisp-mode))

(add-to-list 'auto-mode-alist '("\\.svg$" . xml-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; (enh-)ruby-mode
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)$" . ruby-mode))

;; rgrep
(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))


;; sr-speedbar

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images nil
      speedbar-indentation-width 1
      speedbar-use-imenu-flag t
      speedbar-file-unshown-regexp "flycheck-.*"
      sr-speedbar-width 35
      sr-speedbar-width-x 35
      sr-speedbar-max-width 35
      sr-speedbar-auto-refresh nil
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

(if (eq system-type 'darwin) (setq sr-speedbar-width 25
                                   sr-speedbar-width-x 25
                                   sr-speedbar-max-width 25))

(add-hook 'speedbar-mode-hook
          '(lambda ()
             (hl-line-mode 1)
             (visual-line-mode -1)
             (setq automatic-hscrolling nil)
             (let ((speedbar-display-table (make-display-table)))
               (set-display-table-slot speedbar-display-table 0 8230)
               (setq buffer-display-table speedbar-display-table))))

;; More familiar keymap settings.
(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

;; Always use the last selected window for loading files from speedbar.
(defvar last-selected-window
  (if (not (eq (selected-window) sr-speedbar-window))
      (selected-window)
    (other-window 1)))

(defadvice select-window (after remember-selected-window activate)
  "Remember the last selected window."
  (unless (or (eq (selected-window) sr-speedbar-window)
              (not (window-live-p (selected-window))))
    (setq last-selected-window (selected-window))))

(defun sr-speedbar-before-visiting-file-hook ()
  "Function that hooks `speedbar-before-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-before-visiting-tag-hook ()
  "Function that hooks `speedbar-before-visiting-tag-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-file-hook ()
  "Function that hooks `speedbar-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-tag-hook ()
  "Function that hooks `speedbar-visiting-tag-hook'."
  (select-window last-selected-window))

(dolist (face (list 'speedbar-button-face
                    'speedbar-file-face
                    'speedbar-directory-face
                    'speedbar-tag-face
                    'speedbar-selected-face
                    'speedbar-highlight-face))
  (if (eq window-system 'x)
      (set-face-font face "Monospace-9")
    (if (eq system-type 'darwin)
        (set-face-font face "Menlo 12"))))


;; BUFFER LAYOUT
(defun toggle-window-dedication (window)
  (let* ((dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))
(defun toggle-current-window-dedication ()
  (interactive)
  (let ((window (selected-window)))
    (toggle-window-dedication window))
  )

;; (defun ide ()
;;   (interactive)
;;   (delete-other-windows)

;;   (split-window-horizontally)

;;   (dired-single-magic-buffer code-directory)
;;   (let* ((window (selected-window))
;;          (w (window-width window))
;;          (delta (- 30 w)))
;;     (select-window window)
;;     (enlarge-window-horizontally delta)
;;     (setq window-size-fixed 'width)
;;     (toggle-window-dedication window))

;;   (other-window 1)
;;   (split-window-horizontally)

;; )


;; KEYBINDINGS

(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-S-l") 'recenter-top-bottom)
(global-set-key (kbd "C-<tab>") 'indent-region)
(global-set-key (kbd "C-S-<tab>") 'unindent-region)
(global-set-key (kbd "C-S-<iso-lefttab>") 'unindent-region)
(global-set-key (kbd "C-n") 'next-error)
(global-set-key (kbd "C-p") 'previous-error)
(global-set-key (vector (list 'control mouse-wheel-down-event)) 'text-scale-increase)
(global-set-key (vector (list 'control mouse-wheel-up-event))   'text-scale-decrease)

; various mode keybindings
(global-set-key (kbd "C-S-m") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "C-S-t") 'projectile-find-file)
(global-set-key (kbd "C-S-f") 'rgrep)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (import-js tide yasnippet yaml-mode web-mode typescript-mode sublimity sr-speedbar smex scss-mode sass-mode rvm rubocop robe rainbow-mode projectile php-mode nvm monokai-theme markdown-mode magit json-mode ido-vertical-mode grizzl git-timemachine git-gutter fringe-helper flycheck flx-ido enh-ruby-mode editorconfig drag-stuff dired-details+ company coffee-mode buffer-move autopair auto-complete anything-git-files ac-js2))))


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
