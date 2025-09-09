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

(provide 'global)

;;; global.el ends here
