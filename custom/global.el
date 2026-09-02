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

;; Install a missing global npm CLI once, in the background.  asdf keeps
;; global node binaries behind a shim, so reshim or the new binary stays
;; invisible until the next shell.
(defun /ensure-npm-global (executable package)
  "Install PACKAGE with `npm install --global' when EXECUTABLE is missing."
  (let ((npm (executable-find "npm")))
    (cond
     ((executable-find executable) nil)
     ((not npm)
      (message "/ensure-npm-global: %s is missing and npm was not found"
               executable))
     (t
      (message "/ensure-npm-global: installing %s ..." package)
      (set-process-sentinel
       (start-process (format "npm-install-%s" executable)
                      (format "*npm-install-%s*" executable)
                      npm "install" "--global" package)
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (if (/= 0 (process-exit-status proc))
               (message "/ensure-npm-global: %s failed, see %s"
                        package (buffer-name (process-buffer proc)))
             (when (executable-find "asdf")
               (call-process "asdf" nil nil nil "reshim" "nodejs"))
             (message "/ensure-npm-global: installed %s" package)))))))))

;; Clear the path for aerospace on macos
(global-unset-key (kbd "s-<left>"))
(global-unset-key (kbd "s-<right>"))
(global-unset-key (kbd "s-<up>"))
(global-unset-key (kbd "s-<down>"))
(global-unset-key (kbd "s-f"))

(provide 'global)

;;; global.el ends here
