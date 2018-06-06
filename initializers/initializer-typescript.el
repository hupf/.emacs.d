;; Zwischenstand:

;; indentSize wird nicht übernommen von tide-format-options
;; tide-mode wird nicht geladen (nur typescript-mode)
;; flycheck nicht aktiv?

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
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(import\s+{[^}]+}\s+from\s+\\)\"\\([^\"]*\\)\"\\(;?\\)" nil t)
    (replace-match "\\1'\\2'\\3" nil nil))
  )

(defun typescript-fix-angular-imports ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "import {\\([^}]*\\)} from '@angular\\(/[^/']+\\)\\2';" nil t)
    (replace-match "import {\\1} from '@angular\\2';" nil nil))
  )




;; ;; TypeScript:
;; ;;
;; ;; setup tide mode, the typescript IDE for Emacs
;; ;; This is lifted straight from the suggested setup on the TIDE
;; ;; README https://github.com/ananthakumaran/tide
;; (defun fs/setup-tide-mode()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))

;; (add-hook 'typescript-mode-hook #'fs/setup-tide-mode)

;; ;; setup formatting options. The full list can be found at
;; ;; https://github.com/Microsoft/TypeScript/blob/87e9506/src/services/services.ts#L1244-L1272
;; (setq tide-format-options
;;       '(:indentSize 2 :tabSize 2))

(provide 'initializer-typescript)
