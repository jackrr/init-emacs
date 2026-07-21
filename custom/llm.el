;; -*- lexical-binding: t -*-

;;; llm.el --- LLM tooling stuff

;;; Commentary:

;;; Code:

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
	(setq claude-code-ide-terminal-backend 'ghostel)) ; Optionally enable Emacs MCP tools

(provide 'llm)
;;; llm.el ends here
