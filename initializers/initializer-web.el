;;; initializer-web.el --- Setup HTML editing
;;;
;;; Commentary:
;;;
;;; Modes and configurations for HTML/HAML editing
;;;
;;; Code:

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

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package php-mode
  :mode "\\.php\\'")

(provide 'initializer-web)
;;; initializer-web.el ends here
