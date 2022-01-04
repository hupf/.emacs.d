;;; initializer-web.el --- Setup web editing
;;;
;;; Commentary:
;;;
;;; Modes and configurations for HTML/CSS editing
;;;
;;; Code:

;; Cascading Stylesheets
(use-package css-mode
  :init (setq css-indent-offset 2)
  :mode "\\.css\\'"
  :hook (css-mode . setup-node-lsp))

;; SASS
(use-package scss-mode
  :mode ("\\.scss\\'"
         "\\.sass\\'")
  :hook (scss-mode . setup-node-lsp)

  :init
  ;; Don't auto compile SASS files
  (setq scss-compile-at-save nil))

;; Color highlighting in stylesheet modes
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (css-mode scss-mode web-mode js-mode))

;; HTML
(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing t)

  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.hbs\\'")

  :hook (web-mode . setup-node-lsp)

  :bind (:map web-mode-map
         ("C-/" . web-mode-comment-or-uncomment))
)

;; Enable Emmet mode: http://emmet.io/
;; Emmet is a plugin for many popular text editors which greatly improves HTML & CSS workflow
(use-package emmet-mode
  :init (setq emmet-indentation 2)
  :hook (web-mode sgml-mode))

;; HAML
(use-package haml-mode
  :mode "\\.haml\\'")

(provide 'initializer-web)
;;; initializer-web.el ends here
