;; -*- lexical-binding: t -*-

;;; global.el --- Global emacs settings

;;; Commentary:
; Generally from: https://www.masteringemacs.org/article/disabling-prompts-emacs

;;; Code:
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;; Free up shift+space binding
(setq shift-select-mode nil)

(use-package exec-path-from-shell
  :straight t
  :config
  ;; In a daemon, `window-system' is nil at init time, so guard on
  ;; `daemonp' too or the PATH import silently never runs.
  (when (or (daemonp) (memq window-system '(mac ns x pgtk)))
    (exec-path-from-shell-initialize)))

;; Clear the path for aerospace on macos
(global-unset-key (kbd "s-<left>"))
(global-unset-key (kbd "s-<right>"))
(global-unset-key (kbd "s-<up>"))
(global-unset-key (kbd "s-<down>"))
(global-unset-key (kbd "s-f"))

(provide 'global)

;;; global.el ends here
