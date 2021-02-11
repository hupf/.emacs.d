;;; initializer-completion --- Setup completion
;;;
;;; Commentary:
;;;
;;; Setup of mini buffer and text completion
;;;
;;; Code:

;; Minibuffer completion (ivy/ido alternative)
(use-package selectrum
  :defer 0
  :config (selectrum-mode t))

;; Improved selectrum sorting/filtering
(use-package selectrum-prescient
  :after selectrum

  :custom
  ;; Enable fuzzy matching as well
  (prescient-filter-method '(literal regexp initialism fuzzy))

  :config
  ;; To make sorting and filtering more intelligent
  (selectrum-prescient-mode t)

  ;; To save command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode t))

;; Improved C-s search (isearch/swiper alternative)
(use-package ctrlf
  :disabled
  :defer 0

  :custom
  ;; Don't show match count within content on current line
  (ctrlf-show-match-count-at-eol nil)

  :config
  ;; Wrap all functions bound to keys
  (advice-add 'ctrlf-forward-literal :around #'ctrlf-with-minibuffer-focus)
  (advice-add 'ctrlf-backward-literal :around #'ctrlf-with-minibuffer-focus)
  (advice-add 'ctrlf-forward-regexp :around #'ctrlf-with-minibuffer-focus)
  (advice-add 'ctrlf-backward-regexp :around #'ctrlf-with-minibuffer-focus)
  (advice-add 'ctrlf-forward-symbol :around #'ctrlf-with-minibuffer-focus)
  (advice-add 'ctrlf-forward-symbol-at-point :around #'ctrlf-with-minibuffer-focus)
  (advice-add 'ctrlf-backward-literal :around #'ctrlf-with-minibuffer-focus)

  ;; kill-whole-line should kill line in buffer, not minibuffer
  (add-to-list
   'ctrlf-minibuffer-bindings
   '("<C-S-backspace>" . ctrlf-exit-and-kill-whole-line) t)

  (ctrlf-mode t))

(defun ctrlf-with-minibuffer-focus (orig-fun &rest args)
  "Wrap the given function ORIG-FUN with ARGS and focus minibuffer if search is active but cursor in buffer."
  (interactive)
  (if (and ctrlf--active-p (not (minibufferp (current-buffer))))
      ;; In buffer but with active search, so only focus minibuffer
      (select-window (active-minibuffer-window))
    ;; Original behavior (call wrapped ctrlf function)
    (apply orig-fun args)))

(defun ctrlf-exit-and-kill-whole-line ()
  "Kill whole line of buffer after exiting ctrlf session."
  (interactive)
  ;; Schedule (kill-whole-line) to command loop to be able to execute
  ;; it after the minibuffer has been closed, more details:
  ;; https://oremacs.com/2015/07/16/callback-quit/
  (run-at-time nil nil 'kill-whole-line)
  (exit-minibuffer))

;; Autocomplete
(use-package company
  :defer 0
  :diminish company-mode

  :init
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        ;; invert the navigation direction if the the completion
        ;; popup-isearch-match is displayed on top (happens near the
        ;; bottom of windows)
        company-tooltip-flip-when-above t

        ;; Align annotations to the right tooltip border
        company-tooltip-align-annotations t)

  :config
  (global-company-mode t))

;; Fancy company dialog with icons
(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

(provide 'initializer-completion)
;;; initializer-completion.el ends here
