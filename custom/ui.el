;;; ui.el --- Vis stuff

;;; Commentary:

;;; Code:

(use-package doom-themes
  :straight t
	:bind
	("C-h t" . load-theme)
	:init
	(load-theme 'doom-monokai-octagon t)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))

(use-package solaire-mode
	:straight t
	:config
	(solaire-global-mode +1))

(use-package ultra-scroll
	:straight t
	:init
	(setq scroll-margin 0
				scroll-conservatively 3
				pixel-scroll-precision-interpolate-mice t)
	:config
	(ultra-scroll-mode 1))

(use-package ligature
	:straight t
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
	:straight t
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

(defun font-exists-p (font) (if (null (x-list-fonts font)) nil t))

(when (and window-system (font-exists-p "FiraCode Nerd Font"))
	(set-frame-font "FiraCode Nerd Font 12" nil t))

(use-package nerd-icons
	:straight t)

(use-package neotree
	:straight t
	:bind ("C-c t" . neotree-toggle)
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
