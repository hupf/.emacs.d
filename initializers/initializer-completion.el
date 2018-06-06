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

;; Ivy will use flx for fuzzy matching and sorting if it's installed.
(use-package flx
  :ensure t)

;; Ivy will use smex for the counsel-M-x (it's replacement for M-x) if
;; it is installed.
(use-package smex
  :ensure t)

;; use Ivy mode for completion
(use-package ivy
  :ensure t
  :diminish ivy-mode

  :init
  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; Make the default completion mechanism a fuzzy search. However, you
  ;; don't really want to use fuzzy matching on lists that have content
  ;; with (and ) lot of spaces (like documents), so disable for swiper.
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-count-format "")

  :after (flx smex)
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

;; setup company mode for autocomplete
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

(provide 'initializer-completion)
