;;; finding.el --- Search and completion stuff

;;; Commentary:

;;; Code:
(use-package ag
  :straight t)

(use-package ivy
  :straight t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
	(setq ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  :bind
  ("C-s" . swiper-isearch)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("M-y" . counsel-yank-pop)
  ("C-x b" . ivy-switch-buffer)
  ("C-c v" . ivy-push-view)
  ("C-c V" . ivy-pop-view))

(use-package counsel
  :straight t)

(use-package company
	:straight t
	:bind
	("C-<tab>" . company-complete)
	:config
	(setq company-idle-delay 0.1
				company-minimum-prefix-length 1)
	;; :init
	;; (global-company-mode)
	)
;; https://github.com/joaotavora/eglot/discussions/1436
(add-hook 'after-init-hook 'global-company-mode)

;; Cannot use corfu :sob: because it crashes w/ LSP
;; (use-package corfu
;;   :straight t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                 ; Allows cycling through candidates
;;   (corfu-auto t)                  ; Enable auto completion
;;   (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
;;   (corfu-auto-delay 0)            ; No delay for completion
;;   (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
;;   (corfu-preview-current 'insert) ; insert previewed candidate
;;   (corfu-preselect 'prompt)
;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;; 							;; ("M-SPC"      . corfu-insert-separator)
;; 							;; ("TAB"        . corfu-next)
;; 							;; ([tab]        . corfu-next)
;; 							;; ("S-TAB"      . corfu-previous)
;; 							;; ([backtab]    . corfu-previous)
;; 							;; ("S-<return>" . corfu-insert)
;; 							("RET"        . corfu-insert))
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode) ; Popup completion info
;;   :config
;;   (add-hook 'eshell-mode-hook
;; 						(lambda () (setq-local corfu-quit-at-boundary t
;;                                    corfu-quit-no-match t
;;                                    corfu-auto nil)
;; 							(corfu-mode))
;; 						nil
;; 						t))


(provide 'finding)

;;; finding.el ends here
