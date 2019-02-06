;; GENERAL
(if (functionp 'tool-bar-mode) (tool-bar-mode -1)) ;; no toolbar
(if (functionp 'menu-bar-mode) (menu-bar-mode -1)) ;; no menubar

(setq case-fold-search t) ;; search insensitive

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
    ac-js2

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


;; autocomplete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)


;;autopair
(autopair-global-mode)
(add-hook 'minibuffer-setup-hook
          (lambda () (autopair-mode -1)))


;; git-gutter
(global-git-gutter-mode 1)


;; MODE ASSOCIATIONS
(setq default-major-mode 'text-mode) ;; default mode is text-mode
(add-to-list 'auto-mode-alist '("\\.svg$" . xml-mode))

;; rgrep
(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))


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

;; KEYBINDINGS

(global-set-key (vector (list 'control mouse-wheel-down-event)) 'text-scale-increase)
(global-set-key (vector (list 'control mouse-wheel-up-event))   'text-scale-decrease)

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
