;;; initializer-doc --- Setup markup modes
;;;
;;; Commentary:
;;;
;;; Setup of markup/documentation modes
;;;
;;; Code:

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; Asciidoc
(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))

(provide 'initializer-doc)
;;; initializer-doc.el ends here
