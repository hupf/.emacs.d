;; -- Ivy setup --
;; https://github.com/abo-abo/swiper
;; Ivy has a lot of different moving parts which can be unecessarily
;; confusing. There is (i think) Ivy, the completion system which is
;; just the low-level set of apis for turning text into a set of
;; matches. Then there is Swiper, the UI that interacts with matching
;; in the mini-buffer. And then finally, Counsel, which provides and
;; extends a bunch of core emacs commands. So Ivy, Swiper and Counsel,
;; are all packages that are part of the same system, so we'll refer
;; to them as Ivy from here on out, even though it might technically
;; refer to a sub-part.

;; Minibuffer completion (ivy/ido alternative)
(use-package selectrum
  :ensure t
  :config (selectrum-mode +1))

;; Better selectrum sorting/filtering
(use-package selectrum-prescient
  :after selectrum
  :defer 1

  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

;; Improved search (swiper alternative)
(use-package ctrlf
  :ensure t
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
   (global-company-mode 1))

;; Fancy company dialog with icons
(use-package company-box
  :ensure t
  :diminish
  :hook (company-mode . company-box-mode))

(provide 'initializer-completion)
