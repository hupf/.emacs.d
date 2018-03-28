;;; initializer-style.el --- Setup how Emacs looks.
;;;
;;; Commentary:
;;;
;;; There are three thing that make a good editor good: Capability,
;;; Performance and Style.  Each of which is critical.  It's the last of
;;; these, Style, that this module is all about.
;;;
;;; Let's make it look good!
;;;
;;; Code:

(use-package monokai-theme
  :ensure t
  :init (setq monokai-distinct-fringe-background t)
  :config (load-theme 'monokai t))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(provide 'initializer-style)
;;; initializer-style.el ends here
