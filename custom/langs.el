;; -*- lexical-binding: t -*-

;;; langs.el --- LSP, treesitter and language support
;;; Commentary:

;;; Code:

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'always)
  (global-treesit-auto-mode))

;; Note: treesit-auto handles grammar installation automatically.
;; treesit-auto-install-grammar and treesit-enabled-modes were EMACS-31 preview features
;; that were merged upstream into Emacs 30+ and are no longer needed as standalone functions.
;; Let's just use (use-package treesit-auto ...) config above to handle it.

(defvar /langs-gc-threshold 100000000)

;;; Commentary:

;;; Code:



(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
							("M-n" . flycheck-next-error) ; optional but recommended error navigation
							("M-p" . flycheck-previous-error)))

;; added these for lsp. do these also apply to eglot?
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb

(use-package eglot
	:ensure t
	:hook (((elisp-mode
					 json-mode
					 markdown-mode
					 python-ts-mode
					 rust-ts-mode
					 typst-ts-mode
					 svelte-mode
					 typescript-ts-mode
					 tsx-ts-mode
           yaml-mode) . eglot-ensure))
	:bind (:map eglot-mode-map
							("C-c c d" . xref-find-definitions)
							("C-c c r" . xref-find-references)
							("C-c c a" . eglot-code-actions)
							("C-c c e" . flymake-goto-next-error))
  :custom
	(eglot-autoshutdown t)
	:config
	;; Depends on https://github.com/thefrontside/lspx binary to run
	;; multiple lsp for a buffer
  (add-to-list 'eglot-server-programs
							 '(markdown-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
							 '(svelte-mode . ("bun" "x" "svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
							 '(yaml-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
							 '(tsx-ts-mode . ("bun" "x" "typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
							 '(typescript-mode . ("bun" "x" "typescript-language-server" "--stdio")))
	(add-to-list 'eglot-server-programs
							 '(typescript-ts-mode . ("bun" "x" "typescript-language-server" "--stdio")))
	(add-to-list 'eglot-server-programs
							 '(typst-ts-mode . ("lspx" "--lsp" "tinymist" "--lsp" "harper-ls --stdio"))))

(use-package markdown-mode
  :ensure t)


(use-package svelte-mode
  :ensure t
  :defer t  ;; add this if not present
  :config
  ;; fix face inheritance cycle
  (with-eval-after-load 'gnus
    (require 'svelte-mode)))

;; Needed for svelte mode, no treesitter support for svelte (at this time)
;; (use-package typescript-mode
;; 	:ensure t)

;; (setq auto-mode-alist
;; 			(append
;; 			 '(("\\.tsx\\'" . tsx-ts-mode))
;; 			 auto-mode-alist))

(use-package pyvenv
	:ensure t
	:hook (python-ts-mode . pyvenv-activate-projectile))

;; Haskell / tidal / supercollider
;; (use-package sclang
;; 	:ensure '(sclang :type git :host github :repo "supercollider/scel" :files ("el/*.el")))

;; (use-package haskell-ts-mode
;; 	:ensure t)

(use-package tidal
	:ensure t)

(use-package clojure-mode
	:ensure t)

(use-package cider
	:ensure t)

;; (use-package go-ts-mode)

;; (use-package ruby-ts-mode)

(use-package dockerfile-mode
	:ensure t)

(use-package docker-compose-mode
	:ensure t)

(use-package lua-mode
	:ensure t)

(use-package wgsl-mode
	:ensure t)

(use-package nix-mode
	:ensure t)

(use-package pandoc-mode
	:ensure t
	:hook ((markdown-mode . pandoc-mode))
	;; FIXME: These don't work...
	;; :config
	;; (setq pandoc-options '("--filter" "mermaid-filter"))
	;; (add-hook 'pandoc-mode-hook
	;; (lambda () (pandoc-set 'filter "mermaid-filter"))
	)

(use-package mermaid-mode
	:ensure t)

; From https://www.masteringemacs.org/article/evaluating-elisp-emacs
(defun mp-elisp-mode-eval-buffer ()
	"Evaluate elisp buffer with feedback."
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(defun pyvenv-activate-projectile ()
	"Activates virtualenv via pyvenv at projectile project root for buffer."
	(interactive)
	(pyvenv-activate (concat (projectile-project-root) ".venv")))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

(provide 'langs)
;;; langs.el ends here
