;; GENERAL
(if (functionp 'tool-bar-mode) (tool-bar-mode -1)) ;; no toolbar
(if (functionp 'menu-bar-mode) (menu-bar-mode -1)) ;; no menubar

;; SCROLLING
(when (eq window-system 'x)
    (set-scroll-bar-mode 'right) ;; scrollbar right
)

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

(defun get-project-root (buf repo-type)
  (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))
(defun current-buffer-project-root ()
  (get-project-root (current-buffer ".git")))


;; PACKAGES
(setq package-list '(

    ;; editor features
    auto-complete
    git-timemachine

    ;; javascript
    ac-js2

))

;; MODE OPTIONS

;; dired
(add-hook 'dired-mode-hook 'auto-revert-mode)
(put 'dired-find-alternate-file 'disabled nil)


;; autocomplete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

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


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
