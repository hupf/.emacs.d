;;; initializer-helpers.el --- Helper functions
;;;
;;; Commentary:
;;;
;;; Lisp utility functions
;;;
;;; Code:

(defun camelize-kebab (s)
  "Convert kebab-case string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s "-")) ""))

(provide 'initializer-helpers)
;;; initializer-helpers.el ends here
