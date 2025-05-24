;;; nav.el --- Nav stuff

;;; Commentary:

;;; Code:
(use-package perspective
	:straight t
	:bind
	(("C-x C-b" . persp-list-buffers)
	 ("C-c w w" . persp-next))
	:custom
	((persp-mode-prefix-key (kbd "C-c w"))
	 (persp-interactive-completion-function 'ivy-completing-read))
	:init
	(persp-mode))

(use-package which-key
  :straight t
  :config
  (which-key-mode +1))

(use-package projectile
  :straight t
	:init
	(projectile-mode +1)
	:bind (:map projectile-mode-map
							("C-c p" . projectile-command-map)))

(use-package magit
  :straight t)

(use-package dashboard
  :straight t
  :init
  (setq dashboard-projects-backend 'projectile)
  :config
  (dashboard-setup-startup-hook))


;; (use-package obsidian
;;   :straight t
;;   :config
;;   (global-obsidian-mode t)
;;   (obsidian-backlinks-mode t)
;;   :custom
;;   (obsidian-directory "~/Documents/obsidian"))

;; Want to try, but don't want to give these rich as fuck AI companies
;; more money for an API key. They're already getting my data...
;; (use-package aidermacs
;; 	:straight t
;;   :bind ("C-c a" . aidermacs-transient-menu)
;;   :custom
;; 	;; See the Configuration section below
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-default-model "openai/o4-mini")
;; 	)

;; Allow backups, but store away from source code
(setq backup-directory-alist '((".*" . "~/.config/emacs/backups")))
;; Allow lock files, but store in tmp away from source code
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(defun init-config-edit ()
  "Shortcut to edit init.el."
  (interactive)
  (find-file user-init-file))

(defun launch-tidal (&optional file)
	"Launch supercollider with superdirt and launch a tidal server for FILE."
	(interactive)
	(persp-switch "tidal")
	(let ((default-directory "~/projects/tidal/")
				(tidal-entrypoint (or file "entry.tidal")))
		;; Start sclang w/ duperdirt configuration
		(find-file "bootstrap.sc")
		(sclang-start)
		(sit-for 2)
		;; (kill-buffer "*SCLang:Workspace*")
		(sclang-eval-document)
		(sit-for 3) ;; wait for superdirt to load up

		(find-file tidal-entrypoint)
		(tidal-start-haskell)

		(delete-other-windows)
		(split-window-horizontally)

		;; tidal on left
		(switch-to-buffer tidal-entrypoint)

		;; sclang top right, tidal server bottom right
		(other-window 1)
		(split-window-vertically)
		(switch-to-buffer "*SCLang:PostBuffer*")
		(other-window 1)
		(switch-to-buffer "*tidal*")

		;; back to tidal
		(other-window 1)))

(global-set-key (kbd "C-M-t") 'launch-tidal)

(defun open-project ()
	"Projectile switch project, but opens in new perspective."
	(interactive)
	(projectile-switch-project)
	(let ((proj (projectile-project-name))
				(proj-buffer (buffer-name)))
		(persp-switch proj)
		(persp-set-buffer proj-buffer)
		(switch-to-buffer proj-buffer)
		;; (neotree-toggle)
		(switch-to-buffer proj-buffer)))

(global-set-key (kbd "C-M-o") 'open-project)

(global-set-key (kbd "M-t") 'eshell)

(provide 'nav)

;;; nav.el ends here
