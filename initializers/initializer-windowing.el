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
(use-package hlinum
  :ensure t
  :config (hlinum-activate))

(use-package linum
  :ensure t
  :init (setq linum-format " %3d ")
  :config (add-hook 'prog-mode-hook 'linum-mode))
  ;; :hook prog-mode)

; TODO
;; ;; disable window-system in terminal mode
;; (unless window-system
;;   (menu-bar-mode -1))

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable scratch message
(setq initial-scratch-message nil)

;; ;; War and scrollbars. what are they good for?
;; (require 'scroll-bar)
;; (scroll-bar-mode -1)

;; use super (cmd) + arrow keys to switch between visible buffers
(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings 'super))

;; allow to move current buffer
(use-package buffer-move
  :ensure t
  :init (setq buffer-move-behavior 'move)
  :bind
  ("<C-S-up>" . buf-move-up)
  ("<C-S-down>" . buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

;; add directory to buffer names if files with same name are open
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers

(defun kill-this-buffer-unless-dedicated ()
  (interactive)
  (unless (window-dedicated-p (selected-window))
    (kill-this-buffer)))
(global-set-key (kbd "C-x k") 'kill-this-buffer-unless-dedicated)

(defun kill-all-buffers ()
  "Kills all buffers except internal ones with * (e.g. *scratch*)"
  (interactive)
  (require 'cl)
  (mapc 'kill-buffer (remove-if (lambda (name) (string-match "^\*.*\*$" (string-trim name))) (mapcar (function buffer-name) (buffer-list)))))
(global-set-key (kbd "C-x a k") 'kill-all-buffers)

(provide 'initializer-windowing)
