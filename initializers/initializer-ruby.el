;;; initializer-ruby.el --- Setup Ruby editing
;;;
;;; Commentary:
;;;
;;; Modes and configurations for Ruby editing.
;;;
;;; Code:

;; RVM
(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

;; Enhanced Ruby Mode (uses Ruby's Ripper class instead of regular
;; expressions to parse source files)
(use-package enh-ruby-mode
  :ensure t

  :init
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc")

  ;; Don't add utf-8 comment
  (setq enh-ruby-add-encoding-comment-on-save nil)

  :mode ("\\.rb\\'"
         "\\.rake\\'"
         "Rakefile\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'"
         "Guardfile\\'"
         "Capfile\\'"
         "\\.cap\\'"
         "\\.thor\\'"
         "\\.rabl\\'"
         "Thorfile\\'"
         "Vagrantfile\\'"
         "\\.jbuilder\\'"
         "Podfile\\'"
         "\\.podspec\\'"
         "Puppetfile\\'"
         "Berksfile\\'"
         "Appraisals\\'")

  :hook ((enh-ruby-mode . setup-enh-ruby))

  )

(defun setup-enh-ruby ()
  (rvm-activate-corresponding-ruby)

  ;; TODO: what's the benefit?
  ;; enable a REPL process loaded with your
  ;; ruby project that provides lots of code insight.
  ;; https://github.com/nonsequitur/inf-ruby
  ;; (inf-ruby-minor-mode +1)

  ;; CamelCase aware editing operations
  (subword-mode +1)
  )


;; Robe
(use-package robe
  :ensure t

  :config
  (defadvice inf-ruby-console-auto
    (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))

  :hook ((enh-ruby-mode . robe-mode))

  )


;; Rubocop
(use-package rubocop
  :ensure t
  :defer t

  :init
  (setq rubocop-autocorrect-on-save 1)

  :hook ((enh-ruby-mode . rubocop-mode)))


;; ;; Look up symbols in ruby `ri' to using yari.
;; (define-key 'help-command (kbd "R") 'yari)


;; ;; load snippets for writing tests in rspec whenever rspec-mode
;; ;; is in effect
;; (eval-after-load 'rspec-mode
;;  '(rspec-install-snippets))


;; (custom-set-variables

;;  ;; don't use Rake to run specs. If your suite doesn't run with just
;;  ;; using `bundle exec rspec specs/` then you're doing it wrong.
;;  '(rspec-use-rake-when-possible nil))

(provide 'initializer-ruby)
;;; initializer-ruby.el ends here
