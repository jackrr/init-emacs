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



;; Diagnostics come from eglot -> flymake now (flycheck removed).
(use-package flymake
  :bind (:map flymake-mode-map
							("M-n" . flymake-goto-next-error)
							("M-p" . flymake-goto-prev-error)))

;; flymake backends for modes eglot doesn't cover (each needs its CLI tool).
(use-package flymake-shellcheck
  :ensure t
  :hook (((sh-mode bash-ts-mode) . flymake-shellcheck-load)
         ((sh-mode bash-ts-mode) . flymake-mode)))

(use-package flymake-hadolint
  :ensure t
  :hook ((dockerfile-mode . flymake-hadolint-setup)
         (dockerfile-mode . flymake-mode)))

(use-package flymake-sqlfluff
  :ensure t
  :hook ((sql-mode . flymake-sqlfluff-load)
         (sql-mode . flymake-mode)))

;; luacheck (lua) + yamllint (docker-compose) via flymake-collection.
(use-package flymake-collection
  :ensure t
  :defer t)

(defun /flymake-collection-enable (backend)
  "Load BACKEND (a flymake-collection checker) and turn on flymake here."
  (require backend nil t)
  (add-hook 'flymake-diagnostic-functions backend nil t)
  (flymake-mode 1))

(add-hook 'lua-mode-hook
          (lambda () (/flymake-collection-enable 'flymake-collection-luacheck)))
(add-hook 'docker-compose-mode-hook
          (lambda () (/flymake-collection-enable 'flymake-collection-yamllint)))

;; added these for lsp. do these also apply to eglot?
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb

;; Let pylsp lint via ruff (auto-uses the project's [tool.ruff] config) and
;; disable its built-in pycodestyle/pyflakes/mccabe to avoid duplicates.
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:ruff (:enabled t)
                                  :pycodestyle (:enabled :json-false)
                                  :pyflakes (:enabled :json-false)
                                  :mccabe (:enabled :json-false)))))

(defun /eglot-ts-ls (&optional _interactive _project)
  "Contact for the TypeScript language server, most project-aware first.
Prefer the project's own node_modules binary (matches its pinned
version); else run via bun; else a global typescript-language-server."
  (let* ((from (or buffer-file-name default-directory))
         (nm (and from (locate-dominating-file from "node_modules")))
         (local (and nm (expand-file-name
                         "node_modules/.bin/typescript-language-server" nm))))
    (cond
     ((and local (file-executable-p local)) (list local "--stdio"))
     ((executable-find "bun")
      (list "bun" "x" "typescript-language-server" "--stdio"))
     (t (list "typescript-language-server" "--stdio")))))

(use-package eglot
	:ensure t
	:hook (((elisp-mode
					 json-mode
					 markdown-mode
					 python-mode
					 python-ts-mode
					 rust-ts-mode
					 typst-ts-mode
					 svelte-mode
					 typescript-ts-mode
					 tsx-ts-mode
					 nix-mode
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
							 '(nix-mode . ("nixd")))
	(add-to-list 'eglot-server-programs
							 '((typescript-ts-mode tsx-ts-mode typescript-mode) . /eglot-ts-ls))
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
	:hook ((python-ts-mode python-mode) . pyvenv-activate-nearest-venv))

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

(defun pyvenv-activate-nearest-venv ()
	"Activate the nearest .venv found by walking up from the current buffer.
In a monorepo this picks e.g. backend/.venv for files under backend/,
falling back to the repo-root .venv."
	(interactive)
	(let* ((start (or (and buffer-file-name (file-name-directory buffer-file-name))
										default-directory))
				 (dir (and start (locate-dominating-file start ".venv"))))
		(when dir
			(pyvenv-activate (expand-file-name ".venv" dir)))))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

(provide 'langs)
;;; langs.el ends here
