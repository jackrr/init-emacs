;;; text-manipulation.el --- Working with text ... yah i know

;;; Commentary:

;;; Code:
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :straight t
  :hook ((prog-mode text-mode markdown-mode)
				 ((clojure-mode
					 clojurescript-mode
					 emacs-lisp-mode) . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
	(setq smartparens-global-mode t)
	:bind
	(("C-M-a" . sp-beginning-of-sexp)
	 ("C-M-e" . sp-end-of-sexp)
   ("C-<down>" . sp-down-sexp)
	 ("C-<up>" . sp-up-sexp)
	 ("C-M-<down>" . sp-backward-down-sexp)
	 ("C-M-<up>" . sp-backward-up-sexp)
	 ("C-M-f" . sp-forward-sexp)
	 ("C-M-b" . sp-backward-sexp)
	 ("C-M-n" . sp-next-sexp)
	 ("C-M-p" . sp-previous-sexp)
	 ("M-[" . sp-backward-unwrap-sexp)
	 ("M-]" . sp-unwrap-sexp)
	 ("C-<right>" . sp-forward-slurp-sexp)
	 ("M-<right>" . sp-forward-barf-sexp)
	 ("C-<left>" . sp-backward-slurp-sexp)
	 ("M-<left>" . sp-backward-barf-sexp)
	 ("C-M-t" . sp-transpose-sexp)))

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
	(setf (alist-get 'javascript-mode apheleia-mode-alist)
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
