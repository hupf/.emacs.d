;;; initializer-windowing.el --- Windowing
;;;
;;; Commentary:
;;;
;;; Window and menu bar settings, window/buffer navigation
;;;
;;; Code:

;; TODO
;; ;; Split horizontally when opening a new window from a command
;; ;; whenever possible.
;; (setq split-height-threshold nil)

;; (defun frontside-windowing-adjust-split-width-threshold ()
;;   "Change the value of `split-width-threshold' so that it will cause the screen
;; split once and only once.

;; For example, if the frame is 360 columns wide, then we want the
;; split-width-threshold to be 181. That way, when you split horizontally, the two
;; new windows will each be 180 columns wide, and sit just below the threshold.
;; "
;;   (setq split-width-threshold (+ 1 (/ (frame-width) 2))))

;; ;; recaculate split-width-threshold with every change
;; (add-hook 'window-configuration-change-hook
;;           'frontside-windowing-adjust-split-width-threshold)

;; ;; vertical split, switch window, and open next buffer
;; (defun frontmacs/vsplit-last-buffer ()
;;   (interactive)
;;   (split-window-vertically)
;;   (other-window 1 nil)
;;   (switch-to-next-buffer))
;; (global-set-key (kbd "C-x 2") 'frontmacs/vsplit-last-buffer)

;; ;; horizontal split, switch window, and open next buffer
;; (defun frontmacs/hsplit-last-buffer ()
;;   (interactive)
;;   (split-window-horizontally)
;;   (other-window 1 nil)
;;   (switch-to-next-buffer))
;; (global-set-key (kbd "C-x 3") 'frontmacs/hsplit-last-buffer)

;; Line numbers
;; highlight the current line number
(use-package nlinum
  :ensure t

  :init
  (setq nlinum-format "%3d ")
  (setq nlinum-highlight-current-line t)

  :hook (prog-mode . nlinum-mode)

  :config (set-face-attribute 'linum nil
            :background (face-background 'default))
)

;; Disable menu bar in terminal mode
(unless window-system
  (menu-bar-mode -1))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable scratch message
(setq initial-scratch-message nil)

;; ;; War and scrollbars. what are they good for?
;; (require 'scroll-bar)
;; (scroll-bar-mode -1)

;; Use super (cmd) + arrow keys to switch between visible buffers
(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings 'meta))

;; Allow to move current buffer
(use-package buffer-move
  :ensure t
  :init (setq buffer-move-behavior 'move)
  :bind
  ("<C-S-up>" . buf-move-up)
  ("<C-S-down>" . buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

;; Add directory to buffer names if files with same name are open
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*") ;; Don't muck with special buffers

(defun kill-this-buffer-unless-dedicated ()
  "Kill current buffer, but only if not dedicated."
  (interactive)
  (unless (window-dedicated-p (selected-window))
    (kill-this-buffer)))
(global-set-key (kbd "C-x k") 'kill-this-buffer-unless-dedicated)

(defun kill-all-buffers ()
  "Kill all buffers except internal ones with * (e.g. *scratch*)."
  (interactive)
  (require 'cl)
  (mapc 'kill-buffer (remove-if (lambda (name) (string-match "^\*.*\*$" (string-trim name))) (mapcar (function buffer-name) (buffer-list)))))
(global-set-key (kbd "C-x a k") 'kill-all-buffers)

(provide 'initializer-windowing)
;;; initializer-windowing.el ends here
