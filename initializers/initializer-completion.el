;;; initializer-completion --- Setup completion
;;;
;;; Commentary:
;;;
;;; Setup of mini buffer and text completion
;;;
;;; Code:

;; Minibuffer completion (ivy/ido alternative)
(use-package vertico
  :after orderless

  :custom
  ;; Use grid mode for buffer switching
  (vertico-multiform-commands
   '((switch-to-buffer grid)))

  :init
  (vertico-mode)
  (vertico-multiform-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :defer 0

  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Enable richer minibuffer annotations
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


;; ;; Autocomplete
;; (use-package company
;;   :defer 0
;;   :diminish company-mode

;;   :init
;;   (setq company-idle-delay 0.2
;;         company-tooltip-limit 10
;;         company-minimum-prefix-length 2
;;         ;; invert the navigation direction if the the completion
;;         ;; popup-isearch-match is displayed on top (happens near the
;;         ;; bottom of windows)
;;         company-tooltip-flip-when-above t

;;         ;; Align annotations to the right tooltip border
;;         company-tooltip-align-annotations t)

;;   :config
;;   (global-company-mode t))

;; ;; Fancy company dialog with icons
;; (use-package company-box
;;   :diminish
;;   :hook (company-mode . company-box-mode))

(use-package corfu
  :after orderless

  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))


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


(provide 'initializer-completion)
;;; initializer-completion.el ends here
