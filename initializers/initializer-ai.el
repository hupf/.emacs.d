;;; initializer-ai --- Setup AI tools -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Setup of various AI tools
;;;
;;; Code:

(use-package agent-shell
  :ensure t
  :commands (agent-shell-devin-start)
  :bind (("C-c a d" . agent-shell-devin-start))

  :config
  ;; 1. Define the Devin configuration structure
  (defun agent-shell-devin-make-agent-config ()
    "Create a Devin CLI agent configuration structure."
    (agent-shell-make-agent-config
     :identifier 'devin
     :mode-line-name "Devin"
     :buffer-name "Devin"
     :shell-prompt "Devin> "
     :shell-prompt-regexp "Devin> "
     :icon-name "devin.png"
     :client-maker
     ;; Fix: Accept the mandatory positional buffer argument, ignore trailing keys
     (lambda (buffer &rest _args)
       (unless buffer (error "Missing required argument: buffer"))
       (let ((client (agent-shell--make-acp-client
                      :command "devin"
                      :command-params '("acp")
                      :context-buffer buffer))) ; Pass the buffer context down to the ACP client
         ;; Force unbuffered line output for stdout/stdin
         (when-let ((process (cdr (assoc :process client))))
           (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
           (set-process-query-on-exit-flag process nil))
         client))))

  ;; 2. Define the interactive starter function
  (defun agent-shell-devin-start ()
    "Start an interactive Devin agent shell."
    (interactive)
    (agent-shell--dwim :config (agent-shell-devin-make-agent-config)
                       :new-shell t)))

(provide 'initializer-ai)
;;; initializer-ai.el ends here
