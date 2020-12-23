;;; initializer-completion --- Setup completion
;;;
;;; Commentary:
;;;
;;; Setup of mini buffer and text completion
;;;
;;; Code:

;; Minibuffer completion (ivy/ido alternative)
(use-package selectrum
  :ensure t
  :config (selectrum-mode +1))

;; Improved selectrum sorting/filtering
(use-package selectrum-prescient
  :ensure t
  :after selectrum

  :custom
  ;; Enable fuzzy matching as well
  (prescient-filter-method '(literal regexp initialism fuzzy))

  :config
  ;; To make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; To save command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

;; Improved C-s search (swiper alternative)
(use-package ctrlf
  :ensure t

  :custom
  ;; Don't show match count within content on current line
  (ctrlf-show-match-count-at-eol nil)

  :config (ctrlf-mode +1))

;; Autocomplete
(use-package company
   :ensure t
   :diminish company-mode

   :init
   (setq company-idle-delay 0.2)
   (setq company-tooltip-limit 10)
   (setq company-minimum-prefix-length 2)
   ;; invert the navigation direction if the the completion popup-isearch-match
   ;; is displayed on top (happens near the bottom of windows)
   (setq company-tooltip-flip-when-above t)

   ;; Align annotations to the right tooltip border
   (setq company-tooltip-align-annotations t)

   :config
   (global-company-mode +1))

;; Fancy company dialog with icons
(use-package company-box
  :ensure t
  :diminish
  :hook (company-mode . company-box-mode))

(provide 'initializer-completion)
;;; initializer-completion.el ends here
