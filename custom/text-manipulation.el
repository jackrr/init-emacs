;;; text-manipulation.el --- Working with text ... yah i know

;;; Commentary:

;;; Code:
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :straight t
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  (smartparens-strict-mode 1))

(use-package apheleia
  :straight apheleia
  :config
	;; Apheleia is too opinionated on how to run prettier, just defer to
	;; project config
	(setf (alist-get 'prettier apheleia-formatters)
				'("npx" "prettier" "--stdin-filepath" filepath))
	(setf (alist-get 'haskell-ts-mode apheleia-mode-alist)
				'(ormolu))
	(setf (alist-get 'typescript-mode apheleia-mode-alist)
				'(prettier))
  (apheleia-global-mode +1))

;; Make typing/yanking replace instead of prepend to region
(delete-selection-mode 1)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-<return>") 'comment-indent-new-line)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))


(provide 'text-manipulation)

;;; text-manipulation.el ends here
