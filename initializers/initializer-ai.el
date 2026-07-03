;;; initializer-ai.el --- Setup AI tools -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Setup of various AI tools
;;;
;;; Code:

;; Claude code must be present as well as the claude-agent-acp package. With mise use:
;;  [tools]
;;  node = "26"
;;  "npm:@agentclientprotocol/claude-agent-acp" = "latest"

(use-package agent-shell
  :ensure t

  :bind (("C-c a a" . agent-shell-anthropic-start-claude-code)))

(provide 'initializer-ai)
;;; initializer-ai.el ends here
