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

(defun buffer-file-has-ancestor (file-name)
  "Check whether the file of the current buffer has a file named FILE-NAME in one of its parent directories."
  (let* ((current-directory (expand-file-name (buffer-file-name (current-buffer))))
        (path (locate-dominating-file current-directory file-name)))
    (> (length path) 0)))

(defun copy-current-buffer-filename ()
  "Copy the filename of the current buffer into clipboard."
  (interactive)
  (kill-new (buffer-file-name (current-buffer))))

(provide 'initializer-helpers)
;;; initializer-helpers.el ends here
