;;; initializer-org.el --- Org Mode customizations -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Customizations for the org-mode.
;;;
;;; Code:

(use-package visual-fill-column
  :commands (visual-fill-column-mode visual-line-mode))

(use-package org-present
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t)

  :hook
  (org-present-mode . my/org-present-start)
  (org-present-mode-quit . my/org-present-end))


(defun my/org-present-start ()
  "Center the presentation and wrap lines."
  ;; Tweak font sizes
  ;; (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
  ;;                                    (header-line (:height 4.0) variable-pitch)
  ;;                                    (org-document-title (:height 1.75) org-document-title)
  ;;                                    (org-code (:height 1.55) org-code)
  ;;                                    (org-verbatim (:height 1.55) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block)
  ;;                                    (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  (setq global-hl-line-mode nil)
  (hl-line-mode 0)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  ;; (org-present-big)
  (org-present-hide-cursor)
  (org-present-read-only))

(defun my/org-present-end ()
  "Stop centering the document."
  ;; Reset font customizations
  ;; (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Clear the header line format by setting to `nil'
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  ;; (org-present-small)
  (org-present-show-cursor)
  (org-present-read-write))

;; (defvar my/fixed-width-font "VictorMono Nerd Font Light"
;;   "The font to use for monospaced (fixed width) text.")

;; (defvar my/variable-width-font "IBM Plex Sans"
;;   "The font to use for variable-pitch (document) text.")

;; (set-face-attribute 'default nil :font my/fixed-width-font :weight 'light :height 180)
;; (set-face-attribute 'fixed-pitch nil :font my/fixed-width-font :weight 'light :height 190)
;; (set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'light :height 1.3)


;; Load org-faces to make sure we can set appropriate faces
;; (require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
;; (dolist (face '((org-level-1 . 1.2)
;;                 (org-level-2 . 1.1)
;;                 (org-level-3 . 1.05)
;;                 (org-level-4 . 1.0)
;;                 (org-level-5 . 1.1)
;;                 (org-level-6 . 1.1)
;;                 (org-level-7 . 1.1)
;;                 (org-level-8 . 1.1)))
;;   (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))


;; Make the document title a bit bigger
;; (set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(provide 'initializer-org)
;;; initializer-org.el ends here
