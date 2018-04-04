(use-package web-mode
  :ensure t

  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-pairing t)

  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.hbs\\'")

  :bind ("C-/" . web-mode-comment-or-uncomment)
)

;; Enable Emmet mode: http://emmet.io/
;; Emmet is a plugin for many popular text editors which greatly improves HTML & CSS workflow
(use-package emmet-mode
  :ensure t
  :init (setq emmet-indentation 2)
  :hook (web-mode sgml-mode))

(provide 'initializer-web)
