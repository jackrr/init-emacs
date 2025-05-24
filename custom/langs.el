;;; langs.el --- LSP, treesitter and language support

;;; Commentary:

;;; Code

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
				 ("\\.json\\'" . js-json-mode)
				 ("\\.jsonc\\'" . js-json-mode)
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
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
							 (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
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
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
						 (rust-mode . rust-ts-mode)
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

;; LSP performance tuning
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
;; Increase delay of LSP UI for further perf
;; (setq lsp-idle-delay 0.500)

(use-package lsp-mode
  :diminish "LSP"
  :straight t
  :init
	(setq lsp-keymap-prefix "C-c c")
  :hook ((lsp-mode . lsp-diagnostics-mode)
				 (lsp-mode . lsp-enable-which-key-integration)
				 ((svelte-mode
					 tsx-ts-mode
					 typescript-ts-mode
					 js-ts-mode
					 haskell-tidal-mode
					 haskell-ts-mode
					 rust-ts-mode
					 ;; clojure-mode
					 go-ts-mode
					 ruby-ts-mode
					 clojurescript-mode) . lsp-deferred))
	:commands (lsp lsp-deferred)
	:bind
	("C-c c d" . lsp-find-definition)
  :custom
  ;; (lsp-completion-provider :none)       ; Uncomment if using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))

  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  ;; (lsp-enable-xref t)                   ; Use xref to find references
  ;; (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  ;; (lsp-enable-dap-auto-configure t)     ; Debug support
  ;; (lsp-enable-file-watchers nil)
  ;; (lsp-enable-folding nil)              ; I disable folding since I use origami
  ;; (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  ;; (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  ;; debugging (slows performance)
  ;; (lsp-log-io t)
	)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :straight t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
							("C-c C-d" . 'lsp-ui-doc-glance))
  ;; :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                ;; evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
	:after lsp-mode
  :init
	(setq lsp-tailwindcss-add-on-mode t
				lsp-tailwindcss-server-version "0.14.8"
				lsp-tailwindcss-skip-config-check t
				lsp-tailwindcss-major-modes
				'(css-ts-mode
					typescript-mode
					typescript-ts-mode
					svelte-mode)))

(use-package lsp-haskell
	:straight t)

(use-package markdown-mode
  :straight t)

(use-package rust-mode
  :straight t
	:config
  (setq rust-format-on-save t)
	:bind
	("C-c C-r" . rust-run))

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

(use-package dockerfile-mode
	:straight t)

(use-package docker-compose-mode
	:straight t)

(provide 'langs)
;;; langs.el ends here
