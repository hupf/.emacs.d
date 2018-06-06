(use-package tide
  :ensure t

  :after (typescript-mode company flycheck)

  :init
  (setq typescript-indent-level 2)

  ;; Setup formatting options, see:
  ;; https://github.com/Microsoft/TypeScript/blob/87e9506/src/services/services.ts#L1244-L1272
  (setq tide-format-options '(:indentSize 2
                              :tabSize 2
                              ;; :caseIndentSize 2
                              ;; :convertTabsToSpaces t
                              ;; :indentStyle 2 ;; Smart indentation
                              ))
  ;; TODO: tide-format-options somehow do not work...

  ;; TODO: necessary to define all js-*/js2-* customizations here as well?

  :bind
  (:map typescript-mode-map
        ("C-c C-r f" . tide-fix)
        ("C-c C-r r s" . tide-rename-symbol))
        ;; ("" . tide-refactor)
        ;; ("" . tide-jump-to-definition) -> M-.
        ;; ("" . tide-jump-back) -> M-,
        ;; ("" . tide-documentation-at-point)

  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
         (tide-post-code-edit . (lambda ()
                                  (typescript-fix-import-quotes)
                                  (typescript-fix-angular-imports)
                                  )))
)

(defun typescript-fix-import-quotes ()
  "Replace double quotes generated by language server."
  (goto-char (point-min))
  (while (re-search-forward "\\(import\s+{[^}]+}\s+from\s+\\)\"\\([^\"]*\\)\"\\(;?\\)" nil t)
    (replace-match "\\1'\\2'\\3" nil nil))
  )

(defun typescript-fix-angular-imports ()
  "Fix imports such as @angular/core/core."
  (goto-char (point-min))
  (while (re-search-forward "import {\\([^}]*\\)} from '@angular\\(/[^/']+\\)\\2';" nil t)
    (replace-match "import {\\1} from '@angular\\2';" nil nil))
  )

(provide 'initializer-typescript)
