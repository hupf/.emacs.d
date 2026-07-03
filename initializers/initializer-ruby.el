;;; initializer-ruby.el --- Setup Ruby editing  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Modes and configurations for Ruby editing.
;;;
;;; Code:

;; RVM
(use-package rvm
  :commands (rvm-use rvm-use-default rvm-activate-corresponding-ruby))

;; Ruby (built-in tree-sitter mode)
(use-package ruby-ts-mode
  :ensure nil
  :init
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc")

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

  :hook ((ruby-ts-mode . setup-ruby-ts)))

(defun setup-ruby-ts ()
  ;; Active correct Ruby version for current buffer
  (rvm-activate-corresponding-ruby)

  ;; Enable language server
  (lsp)

  ;; TODO: what's the benefit?
  ;; enable a REPL process loaded with your
  ;; ruby project that provides lots of code insight.
  ;; https://github.com/nonsequitur/inf-ruby
  ;; (inf-ruby-minor-mode +1)

  ;; CamelCase aware editing operations
  (subword-mode +1)

  ;; Hint: If the Gem for Rubocop or Standard cannot be found, execute `M-! bundle`

  ;; Activate the Rubocop linter/formatter
  (when (buffer-file-has-ancestor ".rubocop.yml")
    ;; Enable mode for autocorrecting on save and keybindings
    (rubocop-mode)

    ;; Enforce selecting of the ruby-rubocop checker
    (setq-local flycheck-disabled-checkers '(ruby ruby-standard)) ;; Don't fall back to ruby or standard checker
    (setq-local flycheck-checker 'ruby-rubocop))

  ;; Activate the Standard linter/formatter
  (when (buffer-file-has-ancestor ".standard.yml")
    ;; Fetch from GitHub repo (not a MELPA package)
    (unless (package-installed-p 'flycheck-standardrb)
      (package-vc-install '(flycheck-standardrb :url "https://github.com/julianrubisch/flycheck-standardrb")))

    ;; Enforce selecting of the ruby-standard checker
    (setq-local flycheck-disabled-checkers '(ruby ruby-rubocop)) ;; Don't fall back to ruby or rubocop checker
    (setq-local flycheck-checker 'ruby-standard)))

;; Rubocop
(use-package rubocop
  :commands (rubocop-mode)

  :init
  (setq rubocop-autocorrect-on-save 1))

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
