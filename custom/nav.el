;; -*- lexical-binding: t -*-

;;; nav.el --- Nav stuff

;;; Commentary:

;;; Code:
(use-package perspective
	:ensure t
	:bind
	(("C-x C-b" . persp-list-buffers)
	 ("C-c w w" . persp-next))
	:custom
	((persp-mode-prefix-key (kbd "C-c w"))
	 (persp-interactive-completion-function 'ivy-completing-read))
	:init
	(persp-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package projectile
  :ensure t
	:demand t
	:bind (:map projectile-mode-map
							("C-c p" . projectile-command-map))
	;; https://github.com/joaotavora/eglot/discussions/1436
	:hook (after-init . projectile-mode)
	:config
	(define-key projectile-command-map (kbd "w") #'projectile-create-worktree)
	(define-key projectile-command-map (kbd "W") #'projectile-delete-worktree))

(use-package magit
  :ensure t)

(use-package magit-todos
	:ensure t
	:after magit
	:config (magit-todos-mode 1))

;; (use-package dashboard
;;   :ensure t
;;   :init
;;   (setq dashboard-projects-backend 'projectile)
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package avy
	:ensure t
	:bind (("C-'" . avy-goto-char-2)))

;; (use-package obsidian
;;   :ensure t
;;   :config
;;   (global-obsidian-mode t)
;;   (obsidian-backlinks-mode t)
;;   :custom
;;   (obsidian-directory "~/Documents/obsidian"))

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

(defun /delete-window (&optional window)
	"Like `delete-window', but also allow deleting a frame's main window
when only side windows (e.g. claude-code-ide's terminal pane) would
remain. Normally Emacs refuses to delete the main window in that case
since side windows require one, so this clears the `window-side'
parameter of any side windows first when WINDOW is the main window."
	(interactive)
	(let ((window (or window (selected-window))))
		(when (eq window (window-main-window))
			(dolist (w (window-list nil 'no-minibuf))
				(when (window-parameter w 'window-side)
					(set-window-parameter w 'window-side nil)
					(set-window-parameter w 'window-slot nil))))
		(delete-window window)))

(global-set-key (kbd "C-x 0") #'/delete-window)

(defun open-project-sessions (root)
	"Land on project ROOT in a new perspective with ghostel + claude-code-ide.
Opens a ghostel terminal in ROOT, and a claude-code-ide session
alongside it (in its own side window per
`claude-code-ide-window-side'). Also records ROOT as a known
projectile project. If the current perspective is still the initial
\"main\" one (i.e. this is the first project launched), renames it
to the project name instead of switching to a new perspective, so
launching from the startup projects list doesn't leave an empty
\"main\" perspective cluttering the list."
	(setq root (file-name-as-directory (expand-file-name root)))
	(projectile-add-known-project root)
	(projectile-save-known-projects)
	(let ((default-directory root)
				(name (projectile-project-name root)))
		(if (equal (persp-current-name) persp-initial-frame-name)
				(persp-rename name)
			(persp-switch name))
		(delete-other-windows)
		(ghostel-project)
		(claude-code-ide)))

(defun open-project (&optional project-path)
	"Switch to a project in a new perspective with ghostel + claude-code-ide.
If PROJECT-PATH is non-nil, switch directly to that project root;
otherwise prompt among known projectile projects."
	(interactive)
	(open-project-sessions
	 (or project-path
			 (completing-read "Switch to project: " projectile-known-projects nil t))))

(defun open-project-sessions-magit (root)
	"Land on project ROOT in a new perspective with ghostel + magit.
Opens a ghostel terminal in ROOT in one vertical pane, and `magit-status'
for ROOT in another vertical pane. Also records ROOT as a known
projectile project. If the current perspective is still the initial
\"main\" one (i.e. this is the first project launched), renames it
to the project name instead of switching to a new perspective, so
launching from the startup projects list doesn't leave an empty
\"main\" perspective cluttering the list."
	(setq root (file-name-as-directory (expand-file-name root)))
	(projectile-add-known-project root)
	(projectile-save-known-projects)
	(let ((default-directory root)
				(name (projectile-project-name root)))
		(if (equal (persp-current-name) persp-initial-frame-name)
				(persp-rename name)
			(persp-switch name))
		(delete-other-windows)
		(ghostel-project)
		(split-window-right)
		(other-window 1)
		(magit-status root)))

(defun open-project-magit (&optional project-path)
	"Switch to a project in a new perspective with ghostel + magit panes.
If PROJECT-PATH is non-nil, switch directly to that project root;
otherwise prompt among known projectile projects."
	(interactive)
	(open-project-sessions-magit
	 (or project-path
			 (completing-read "Switch to project: " projectile-known-projects nil t))))

(defvar projectile-worktree-dir-overrides
	'(("~/dev/app/" . "../"))
	"Alist of (PROJECT-ROOT . RELATIVE-DIR) overriding where
`projectile-create-worktree' puts new worktrees for PROJECT-ROOT.
RELATIVE-DIR is resolved against PROJECT-ROOT, so \"../\" makes
worktrees siblings of the project root instead of nesting them in
\"worktrees/\" (the default when a project has no entry here).
E.g.: (add-to-list \\='projectile-worktree-dir-overrides
                    \\='(\"~/projects/foo/\" . \"../\"))")

(defun /project-worktree-dir (root)
	"Return the worktree parent directory (relative to ROOT) for ROOT."
	(or (cdr (assoc root projectile-worktree-dir-overrides #'file-equal-p))
			"worktrees/"))

(defun /ensure-worktrees-gitignored (root)
	"Add a worktrees/ line to ROOT's .gitignore if it isn't already there."
	(let ((gitignore (expand-file-name ".gitignore" root)))
		(unless (and (file-exists-p gitignore)
								 (with-temp-buffer
									 (insert-file-contents gitignore)
									 (goto-char (point-min))
									 (re-search-forward "^worktrees/?$" nil t)))
			(with-temp-buffer
				(when (file-exists-p gitignore)
					(insert-file-contents gitignore))
				(goto-char (point-max))
				(unless (or (bobp) (bolp))
					(insert "\n"))
				(insert "worktrees/\n")
				(write-region (point-min) (point-max) gitignore)))))

(defvar projectile-worktree-script-overrides
	'(("~/dev/app/" . "~/dev/app/.claude/setup-worktree.sh"))
	"Alist of (PROJECT-ROOT . SCRIPT-PATH). When set for a project,
`projectile-create-worktree' runs SCRIPT-PATH with the worktree name
as its sole argument instead of running `git worktree add' directly.
The script is responsible for creating the worktree at the path
`/project-worktree-dir' would compute.
E.g.: (add-to-list \\='projectile-worktree-script-overrides
                    \\='(\"~/dev/app/\" . \"~/dev/app/.claude/setup-worktree.sh\"))")

(defun /project-root ()
	"Resolve the current project root, erroring clearly if there is none.
Retries once after `projectile-invalidate-cache' — a directory visited
before it had a .git (e.g. a brand new project) leaves a stale nil
cached for it, which would otherwise surface as a cryptic
wrong-type-argument deeper in the worktree commands."
	(or (projectile-project-root)
			(progn (projectile-invalidate-cache nil)
						 (projectile-project-root))
			(user-error "Not inside a recognized projectile project")))

(defun projectile-create-worktree ()
	"Create a git worktree for the current project and open it.
Creates a new branch and worktree named by prompt, under the directory
given by `/project-worktree-dir' (nested \"worktrees/\" by default, or
a per-project override such as \"../\"). When the worktree lands
inside ROOT, also ensures worktrees/ is gitignored there. If
`projectile-worktree-script-overrides' has an entry for ROOT, that
script is run (with NAME as its only argument) instead of `git
worktree add'. Lands on the new worktree via `open-project-sessions'."
	(interactive)
	(let* ((root (file-name-as-directory (/project-root)))
				 (name (read-string "Worktree name: "))
				 (worktrees-dir (expand-file-name (/project-worktree-dir root) root))
				 (wt-path (expand-file-name name worktrees-dir))
				 (script (cdr (assoc root projectile-worktree-script-overrides #'file-equal-p))))
		(when (string-prefix-p (expand-file-name root) (expand-file-name wt-path))
			(/ensure-worktrees-gitignored root))
		(when (and script (not (file-executable-p script)))
			(user-error "Worktree script %s is not executable" script))
		(let ((default-directory root))
			(with-temp-buffer
				(let ((status (if script
													 (call-process script nil t nil name)
												 (call-process "git" nil t nil
																			 "worktree" "add" wt-path "-b" name))))
					(unless (zerop status)
						(user-error "%s failed: %s"
												(if script script "git worktree add")
												(buffer-string))))))
		(open-project-sessions wt-path)))

(defun /project-worktree-list (root)
	"Return git worktree paths for the repo at ROOT, main tree first."
	(let (paths (default-directory root))
		(with-temp-buffer
			(call-process "git" nil t nil "worktree" "list" "--porcelain")
			(goto-char (point-min))
			(while (re-search-forward "^worktree \\(.+\\)$" nil t)
				(push (match-string 1) paths)))
		(nreverse paths)))

(defun projectile-delete-worktree ()
	"Remove one of the current project's git worktrees.
Prompts among worktrees (excluding the main working tree), removes it
via `git worktree remove', deletes its same-named branch if merged,
and cleans up its perspective and known-projects entry."
	(interactive)
	(let* ((root (file-name-as-directory (/project-root)))
				 (all (/project-worktree-list root))
				 (main (file-name-as-directory (expand-file-name (car all))))
				 (others (mapcar (lambda (p) (file-name-as-directory (expand-file-name p)))
												 (cdr all))))
		(unless others
			(user-error "No worktrees to delete for %s" main))
		(let* ((wt-path (completing-read "Delete worktree: " others nil t))
					 (wt-name (file-name-nondirectory (directory-file-name wt-path))))
			(when (yes-or-no-p (format "Delete worktree %s? " wt-path))
				(let ((default-directory main))
					(with-temp-buffer
						(let ((status (call-process "git" nil t nil "worktree" "remove" wt-path)))
							(unless (zerop status)
								(user-error "git worktree remove failed: %s" (buffer-string)))))
					(call-process "git" nil nil nil "branch" "-d" wt-name))
				(projectile-remove-known-project (file-name-as-directory (abbreviate-file-name wt-path)))
				(projectile-save-known-projects)
				(when (member wt-name (persp-names))
					(persp-kill wt-name))
				(message "Deleted worktree %s" wt-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent-projects startup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar recent-projects-buffer-name "*Projects*")

(define-derived-mode recent-projects-mode special-mode "RecentProjects"
	"Major mode for the recent projects landing buffer."
	(setq-local cursor-type 'box
							truncate-lines t))

(defun recent-projects--open-at-point ()
	"Open the project on the current line with ghostel + magit panes."
	(interactive)
	(let ((path (get-text-property (line-beginning-position) 'project-path)))
		(when path
			(open-project-magit path))))

(defun recent-projects--open-at-point-claude ()
	"Open the project on the current line with ghostel + claude-code-ide."
	(interactive)
	(let ((path (get-text-property (line-beginning-position) 'project-path)))
		(when path
			(open-project path))))

(define-key recent-projects-mode-map (kbd "RET") #'recent-projects--open-at-point)
(define-key recent-projects-mode-map (kbd "c")   #'recent-projects--open-at-point-claude)
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
	(when (fboundp 'projectile-cleanup-known-projects)
		(projectile-cleanup-known-projects))
	(let ((buf (get-buffer-create recent-projects-buffer-name)))
		(with-current-buffer buf
			(let ((inhibit-read-only t))
				(erase-buffer)
				(insert (propertize "Recent projects\n\n"
														 'face '(:height 1.4 :weight bold)))
				(insert (propertize "  RET open (ghostel+magit)  ·  c open (ghostel+claude)  ·  n/p move  ·  g refresh  ·  q quit\n\n"
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
(global-set-key (kbd "C-c P") 'recent-projects-show)

(global-set-key (kbd "M-t") 'eshell)

(provide 'nav)

;;; nav.el ends here
