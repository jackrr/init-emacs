;; -*- lexical-binding: t -*-

;;; ui.el --- Vis stuff

;;; Commentary:

;;; Code:

(use-package doom-themes
  :ensure t
	:bind
	("C-h t" . load-theme)
	:init
	(load-theme 'doom-ayu-mirage t)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))

(use-package solaire-mode
	:ensure t
	:config
	(solaire-global-mode +1))

(use-package ultra-scroll
	:ensure t
	:init
	(setq scroll-margin 0
				scroll-conservatively 3
				pixel-scroll-precision-interpolate-mice t)
	:config
	(ultra-scroll-mode 1))

;; C-k in a ghostel terminal: the terminal program (e.g. Claude's prompt)
;; deletes the text into its own kill buffer, which Emacs never sees. Copy
;; the text from the terminal cursor to end of line into the kill ring
;; first, so C-y (`ghostel-yank') and the system clipboard get it too.
(defun my/ghostel-kill-line ()
  "Copy text from the terminal cursor to end of line, then send C-k."
  (interactive)
  (when ghostel--cursor-char-pos
    (let ((text (string-trim-right
                 (buffer-substring-no-properties
                  ghostel--cursor-char-pos
                  (save-excursion
                    (goto-char ghostel--cursor-char-pos)
                    (line-end-position))))))
      (unless (string-empty-p text)
        (kill-new text))))
  (ghostel-send-key "k" "ctrl"))

(use-package ghostel
  :ensure t
	:bind (("C-c t" . ghostel-project)
				 ("C-c C-t" . ghostel-project)
				 :map ghostel-semi-char-mode-map
				 ("C-k" . my/ghostel-kill-line)))

(use-package ligature
	:ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures
	 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures
	 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
	 ;; 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
   ;;              ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
   ;;              "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
   ;;              "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
   ;;              "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
   ;;              "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
   ;;              "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
   ;;              "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
   ;;              ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
   ;;              "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
   ;;              "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
   ;;              "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
   ;;              "\\\\" "://")
	 'prog-mode '("==>" "=/=" "!==" "-->" "---" "<==" "<=>" "<->"
                "<--" "..." "&&" "~=" "~>" "||" "|=" ":=" ":>"
								"==" "=>" "!=" "!!" ">=" ">>" "->" "--" "<~" "<="
								"<>" "<-" "<<" "#=" "#!" "##" "#(" "#?" "#_" "%%"
								".=" ".-" ".." ".?" "?." "??" ";;" "/*" "/=" "/>"
								"//" "~~" "\\\\")
	 )
  ;; Enables ligature checks globally in all buffers.  You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package hl-todo
	:ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-12"))

(use-package nerd-icons
	:ensure t)

(use-package doom-modeline
	:ensure t
	:after nerd-icons
	:init (doom-modeline-mode 1)
	:custom
	;; Keep things short and legible
	(doom-modeline-height 28)
	(doom-modeline-bar-width 3)
	(doom-modeline-icon (display-graphic-p))
	(doom-modeline-major-mode-icon t)
	(doom-modeline-major-mode-color-icon t)
	(doom-modeline-buffer-state-icon t)
	(doom-modeline-buffer-modification-icon t)
	;; Show just the file name, not the full path
	(doom-modeline-buffer-file-name-style 'file-name)
	;; Hide the noisy bits
	(doom-modeline-minor-modes nil)
	(doom-modeline-enable-word-count nil)
	(doom-modeline-buffer-encoding nil)
	(doom-modeline-indent-info nil)
	(doom-modeline-vcs-max-length 20)
	(doom-modeline-lsp nil)
	(doom-modeline-github nil)
	(doom-modeline-mu4e nil)
	(doom-modeline-irc nil)
	(doom-modeline-persp-name t)
	(doom-modeline-display-default-persp-name nil)
	(doom-modeline-persp-icon t))

;; Hide minor-mode lighters from the default modeline as a fallback,
;; in case doom-modeline isn't loaded yet.
(use-package diminish :ensure t)

(use-package neotree
	:ensure t
  :bind (("C-c C-d" . neotree-toggle)
				 ("C-c d" . neotree-toggle))
	:config
	(setq
	 neo-theme (if (display-graphic-p) 'nerd-icons 'arrow)
	 ;; smart-open uses projectile root
	 neo-smart-open t
	 neo-window-position 'right
	 neo-window-width 20
	 neo-mode-line-type 'neotree
	 neo-header-height 3))

(setq-default tab-width 2)

;; Hide menu bar, scroll bar, tool bar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'ui)

;;; ui.el ends here
