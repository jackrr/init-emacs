;;; langs.el --- LSP, treesitter and language support

;;; Commentary:

;;; Code:
(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
				 ("\\.json\\'" . js-json-mode)
         ("\\.jsonc\\'" . js-json-mode)
				 ("\\.rs\\'" . rust-ts-mode)
				 )
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
							 (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
							 (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
							 (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
							 ;; (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.4.0"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
							 (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
							 (typst . ("https://github.com/uben0/tree-sitter-typst"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
							 (svelte . ("https://github.com/tree-sitter-grammars/tree-sitter-svelte" "v1.0.2"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
						 (javascript-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
						 ;; (lua-mode . lua-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
						 (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
						 (haskell-mode . haskell-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :straight t
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("path-to-git-checkout-of-combobulate")))


(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
							("M-n" . flycheck-next-error) ; optional but recommended error navigation
							("M-p" . flycheck-previous-error)))

;; added these for lsp. do these also apply to eglot?
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(use-package eglot
	:straight t
	:hook (((
					 ;; clojure-mode
					 ;; clojurescript-mode
					 elisp-mode
					 json-mode
					 markdown-mode
					 rust-ts-mode
					 typst-ts-mode
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
							 '(yaml-mode . ("harper-ls" "--stdio")))
	(add-to-list 'eglot-server-programs
							 '(typst-ts-mode . ("lspx" "--lsp" "tinymist" "--lsp" "harper-ls --stdio"))))
;; Old lsp mappings:
;; 				 ((svelte-mode
;; 					 tsx-ts-mode
;; 					 typescript-ts-mode
;; 					 js-ts-mode
;; 					 haskell-tidal-mode
;; 					 haskell-ts-mode
;; 					 ;; clojure-mode
;; 					 lua-mode
;; 					 go-ts-mode
;; 					 ruby-ts-mode
;; 					 typst-ts-mode
;; 					 clojurescript-mode) . lsp-deferred))

;; (use-package lsp-eslint
;;   :demand t
;;   :after lsp-mode)

;; (use-package lsp-tailwindcss
;;   :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
;; 	:after lsp-mode
;;   :init
;; 	(setq lsp-tailwindcss-add-on-mode t
;; 				lsp-tailwindcss-server-version "0.14.8"
;; 				lsp-tailwindcss-skip-config-check t
;; 				lsp-tailwindcss-major-modes
;; 				'(css-ts-mode
;; 					typescript-mode
;; 					typescript-ts-mode
;; 					svelte-mode)))

;; (use-package lsp-haskell
;; 	:straight t)

(use-package markdown-mode
  :straight t)

(use-package svelte-mode
	:straight t)

;; Needed for svelte mode, no treesitter support for svelte (at this time)
(use-package typescript-mode
	:straight t)

;; Haskell / tidal / supercollider
(use-package sclang
	:straight '(sclang :type git :host github :repo "supercollider/scel" :files ("el/*.el")))

(use-package haskell-ts-mode
	:straight t)

(use-package tidal
	:straight t)

(use-package clojure-mode
	:straight t)

(use-package cider
	:straight t)

(use-package go-ts-mode)

(use-package ruby-ts-mode)

(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))

(use-package dockerfile-mode
	:straight t)

(use-package docker-compose-mode
	:straight t)

(use-package lua-mode
	:straight t)

(use-package wgsl-mode
	:straight t)

(use-package nix-mode
	:straight t)

(use-package pandoc-mode
	:straight t
	:hook ((markdown-mode . pandoc-mode))
	;; FIXME: These don't work...
	;; :config
	;; (setq pandoc-options '("--filter" "mermaid-filter"))
	;; (add-hook 'pandoc-mode-hook
	;; (lambda () (pandoc-set 'filter "mermaid-filter"))
	)

(use-package mermaid-mode
	:straight t)

; From https://www.masteringemacs.org/article/evaluating-elisp-emacs
(defun mp-elisp-mode-eval-buffer ()
	"Evaluate elisp buffer with feedback."
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

(provide 'langs)
;;; langs.el ends here
