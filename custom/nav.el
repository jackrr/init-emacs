;;; nav.el --- Nav stuff

;; -*- lexical-binding: t -*-

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
	:demand t
	:bind (:map projectile-mode-map
							("C-c p" . projectile-command-map))
	;; https://github.com/joaotavora/eglot/discussions/1436
	:hook (after-init . projectile-mode))

(use-package magit
  :straight t)

(use-package magit-todos
	:straight t
	:after magit
	:config (magit-todos-mode 1))

;; (use-package dashboard
;;   :straight t
;;   :init
;;   (setq dashboard-projects-backend 'projectile)
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package avy
	:straight t
	:bind (("C-'" . avy-goto-char-2)))

;; (use-package obsidian
;;   :straight t
;;   :config
;;   (global-obsidian-mode t)
;;   (obsidian-backlinks-mode t)
;;   :custom
;;   (obsidian-directory "~/Documents/obsidian"))

(use-package aidermacs
	:straight t
  :bind ("C-c a" . aidermacs-transient-menu)
  :custom
	;; See the Configuration section below
  (aidermacs-use-architect-mode t)
  ;; (aidermacs-default-model "sonnet")
	(aidermacs-default-model "ollama_chat/qwen2.5-coder:latest")
	)

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

(defun open-project (&optional project-path)
	"Projectile switch project, but opens in new perspective.
If PROJECT-PATH is non-nil, switch directly to that project root."
	(interactive)
	(if project-path
			(let ((projectile-switch-project-action #'projectile-find-file))
				(projectile-switch-project-by-name project-path))
		(projectile-switch-project))
	(let ((proj (projectile-project-name))
				(proj-buffer (buffer-name)))
		(persp-switch proj)
		(persp-set-buffer proj-buffer)
		(switch-to-buffer proj-buffer)
		;; (neotree-toggle)
		(switch-to-buffer proj-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent-projects startup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar recent-projects-buffer-name "*Projects*")

(define-derived-mode recent-projects-mode special-mode "RecentProjects"
	"Major mode for the recent projects landing buffer."
	(setq-local cursor-type 'box
							truncate-lines t))

(defun recent-projects--open-at-point ()
	"Open the project on the current line."
	(interactive)
	(let ((path (get-text-property (line-beginning-position) 'project-path)))
		(when path
			(open-project path))))

(define-key recent-projects-mode-map (kbd "RET") #'recent-projects--open-at-point)
(define-key recent-projects-mode-map (kbd "n")   #'next-line)
(define-key recent-projects-mode-map (kbd "p")   #'previous-line)
(define-key recent-projects-mode-map (kbd "g")   #'recent-projects-show)
(define-key recent-projects-mode-map (kbd "q")   #'quit-window)

(defun recent-projects-show ()
	"Show a buffer listing recent projectile projects."
	(interactive)
	(require 'projectile)
	;; `projectile-known-projects' is normally loaded when `projectile-mode'
	;; turns on (after-init-hook). This buffer renders earlier than that via
	;; `initial-buffer-choice', so make sure the list is populated.
	(when (fboundp 'projectile-load-known-projects)
		(projectile-load-known-projects))
	(let ((buf (get-buffer-create recent-projects-buffer-name)))
		(with-current-buffer buf
			(let ((inhibit-read-only t))
				(erase-buffer)
				(insert (propertize "Recent projects\n\n"
														 'face '(:height 1.4 :weight bold)))
				(insert (propertize "  RET open  ·  n/p move  ·  g refresh  ·  q quit\n\n"
														 'face 'shadow))
				(let ((projects (and (boundp 'projectile-known-projects)
														 projectile-known-projects)))
					(if (null projects)
							(insert "  (no known projects yet — C-M-o to add one)\n")
						(dolist (path projects)
							(let* ((name (file-name-nondirectory
														 (directory-file-name path)))
										 (line (format "  %-28s  %s\n"
																			 (propertize name 'face 'font-lock-function-name-face)
																			 (propertize (abbreviate-file-name path) 'face 'shadow))))
								(insert (propertize line 'project-path path)))))))
			(recent-projects-mode)
			(goto-char (point-min))
			(forward-line 3))
		(switch-to-buffer buf)))

;; Show the projects buffer at startup instead of *scratch*
(setq initial-buffer-choice
			(lambda ()
				(recent-projects-show)
				(get-buffer recent-projects-buffer-name)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-M-o") 'open-project)

(global-set-key (kbd "M-t") 'eshell)

(provide 'nav)

;;; nav.el ends here
