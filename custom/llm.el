;; -*- lexical-binding: t -*-

;;; llm.el --- LLM tooling stuff

;;; Commentary:

;; Idle notifications: tags every ghostel-spawned terminal (which includes
;; claude-code-ide sessions, since `claude-code-ide-terminal-backend' is
;; `ghostel') with its perspective and a stable per-buffer id via
;; `ghostel-pre-spawn-hook'. The `claude' CLI child process inherits these,
;; and bin/claude-idle-notify (registered as a Claude Code `Notification'
;; hook) reads them back and calls `claude-idle-notify' via emacsclient when
;; an agent goes idle.
;;
;; The id (not the buffer name) is what's threaded through: ghostel's title
;; tracking renames the buffer shortly after spawn (e.g. "*emacs-ghostel*"
;; -> "*ghostel: user@host:path*"), so a name captured at spawn time would
;; no longer resolve by the time a notification fires.
;;
;; Setup: register bin/claude-idle-notify as a Claude Code `Notification'
;; hook in ~/.claude/settings.json (adjust the path if this repo lives
;; elsewhere), and ensure it's executable (chmod +x):
;;
;;   {
;;     "hooks": {
;;       "Notification": [
;;         {
;;           "hooks": [
;;             {
;;               "type": "command",
;;               "command": "/home/jack/.config/emacs/bin/claude-idle-notify"
;;             }
;;           ]
;;         }
;;       ]
;;     }
;;   }
;;
;; On macOS, run bin/make-claude-notifier-app once so notifications are sent
;; by a bundle named "Claude (Emacs)" with the Emacs icon; without it they
;; are sent by terminal-notifier and look like they come from a terminal.
;;
;; Also requires `emacsclient' to reach a running Emacs server -- this repo
;; doesn't call `server-start' itself, so start one (M-x server-start, or
;; run Emacs as `emacs --daemon') -- and `jq' on PATH for the hook script
;; to parse its JSON payload.

;;; Code:

(require 'notifications)
(require 'cl-lib)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
	(setq claude-code-ide-terminal-backend 'ghostel)) ; Optionally enable Emacs MCP tools

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing a Claude prompt in Emacs instead of an external editor.
;;
;; C-g in the Claude Code CLI opens the pending prompt in $EDITOR. Inside a
;; ghostel terminal that inherited an $EDITOR of `code' (or none, in which
;; case the CLI picks whatever GUI editor it finds), that pops a separate
;; app. Point $EDITOR at `emacsclient' so the prompt opens as an ordinary
;; buffer in the current frame -- and so in the current perspective.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'server)

(defvar llm-emacsclient-program
  (or (let ((sibling (expand-file-name "emacsclient" invocation-directory)))
        (and (file-executable-p sibling) sibling))
      (executable-find "emacsclient")
      "emacsclient")
  "The `emacsclient' to hand to child processes as $EDITOR.
Prefers the one shipped next to the running Emacs, so the version
always matches the server.")

(defun llm--tag-spawn-editor ()
  "Make child processes of a ghostel terminal edit inside this Emacs."
  (setenv "EDITOR" llm-emacsclient-program)
  (setenv "VISUAL" llm-emacsclient-program))

(defvar claude-prompt-file-regexp "claude\\|prompt"
  "Match server-visited file names that hold a pending Claude prompt.
Buffers that match get `claude-prompt-mode', which adds the C-c C-c /
C-c C-k keys. Everything else keeps plain `emacsclient' behaviour (C-x #
to finish).")

(defun claude-prompt-send ()
  "Save the prompt and hand it back to the waiting Claude CLI."
  (interactive)
  (save-buffer)
  (server-edit))

(defun claude-prompt-cancel ()
  "Send an empty prompt back to the waiting Claude CLI."
  (interactive)
  (erase-buffer)
  (save-buffer)
  (server-edit))

(defvar claude-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-prompt-send)
    (define-key map (kbd "C-c C-k") #'claude-prompt-cancel)
    map)
  "Keys for finishing a prompt handed over by the Claude CLI.")

(define-minor-mode claude-prompt-mode
  "Minor mode for a Claude prompt opened here through `emacsclient'."
  :lighter " ClaudePrompt"
  :keymap claude-prompt-mode-map
  (when claude-prompt-mode
    (setq-local header-line-format
                (substitute-command-keys
                 "Claude prompt: \\[claude-prompt-send] sends, \\[claude-prompt-cancel] cancels"))))

(defun claude-prompt--maybe-enable ()
  "Turn on `claude-prompt-mode' if this server buffer holds a Claude prompt."
  (when (and buffer-file-name
             (string-match-p claude-prompt-file-regexp
                             (downcase (file-name-nondirectory buffer-file-name))))
    (claude-prompt-mode 1)))

(add-hook 'server-visit-hook #'claude-prompt--maybe-enable)

;; Show handed-over files next to the terminal rather than on top of it.
(setq server-window
      (lambda (buffer)
        (pop-to-buffer buffer '((display-buffer-reuse-window
                                 display-buffer-below-selected)
                                (window-height . 0.4)))))

;; Both $EDITOR and the notification hook below need a live server.
(unless (server-running-p)
  (server-start))

(defun my/agent-shell-opencode-new-shell ()
  "Start a new OpenCode agent shell, skipping the session-strategy prompt."
  (interactive)
  (agent-shell--start :config (agent-shell-opencode-make-agent-config)
                       :new-session t
                       :session-strategy 'new))

(use-package agent-shell
  :ensure t
	:bind (("C-c C-o n" . my/agent-shell-opencode-new-shell)
         ("C-c C-o r" . agent-shell-opencode-start-agent))
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-opencode-default-model-id "ollama/qwen3.8:27b"))

(defvar notify--buffer-id-counter 0)

(defvar-local notify--buffer-id nil
  "Stable identifier for this buffer, set at ghostel spawn time.
Survives later buffer renames, unlike the buffer name itself.")

(defun notify--tag-spawn-environment ()
  "Tag the about-to-be-spawned ghostel process with its Emacs coordinates."
  (setq notify--buffer-id (format "b%d" (cl-incf notify--buffer-id-counter)))
  (setenv "EMACS_PERSP_NAME" (if (fboundp 'persp-current-name)
                                  (persp-current-name)
                                ""))
  (setenv "EMACS_BUFFER_ID" notify--buffer-id))

(with-eval-after-load 'ghostel
  (add-hook 'ghostel-pre-spawn-hook #'notify--tag-spawn-environment)
  (add-hook 'ghostel-pre-spawn-hook #'llm--tag-spawn-editor))

(defun notify--find-buffer-by-id (id)
  "Return the live buffer tagged with ID via `notify--buffer-id', or nil."
  (cl-find-if (lambda (b) (equal (buffer-local-value 'notify--buffer-id b) id))
              (buffer-list)))

(defvar claude-idle-notifications nil
  "Ring of recent idle notifications, newest first.
Each entry is a plist (:time TIME :persp PERSP :buffer-id ID :message MSG).")

(defvar claude-idle-notifications-max 20
  "Maximum number of entries kept in `claude-idle-notifications'.")

(defvar claude-idle-notify-backend-alist
  '((gnu/linux . claude-idle-notify--linux)
    (darwin    . claude-idle-notify--darwin))
  "How to surface an idle notification, chosen per host.

Keys are either a hostname string (`system-name') or a `system-type'
symbol; hostname entries take priority. Each value is a function of
\(PERSP BUFFER-ID BUFFER-NAME MESSAGE) responsible for showing an OS
notification and, where the OS supports it, wiring a click/action back
to `claude-idle-jump'.

To customize a specific machine without touching the defaults, add a
hostname entry, e.g.:
  (add-to-list \\='claude-idle-notify-backend-alist
                (cons (system-name) #\\='my-custom-backend))")

(defun claude-idle-notify--backend ()
  "Resolve the backend function to use on this host."
  (or (alist-get (system-name) claude-idle-notify-backend-alist nil nil #'equal)
      (alist-get system-type claude-idle-notify-backend-alist)
      (lambda (_persp _id _name message) (message "Claude idle: %s" message))))

(defun claude-idle-jump (persp buffer-id)
  "Switch to perspective PERSP and pop to the buffer tagged BUFFER-ID.
Raises the frame. Reports rather than errors if PERSP or the buffer no
longer exist."
  (let ((buffer (notify--find-buffer-by-id buffer-id)))
    (cond
     ((and (not (string-empty-p persp))
           (fboundp 'persp-names)
           (not (member persp (persp-names))))
      (message "claude-idle-jump: perspective %S no longer exists" persp))
     ((not buffer)
      (message "claude-idle-jump: buffer %S no longer exists" buffer-id))
     (t
      (unless (string-empty-p persp)
        (persp-switch persp))
      (pop-to-buffer buffer)
      (raise-frame)
      (select-frame-set-input-focus (selected-frame))
      ;; `raise-frame' alone does not pull Emacs.app in front of the app that
      ;; owns the notification, so ask the window server directly.
      (when (eq system-type 'darwin)
        (call-process "osascript" nil 0 nil
                      "-e" "tell application id \"org.gnu.Emacs\" to activate"))))))

(defun claude-idle-notify--title (persp buffer-name)
  "Notification title for an agent in PERSP (or BUFFER-NAME if unnamed)."
  (format "%s (emacs): claude idle"
          (if (string-empty-p persp) buffer-name persp)))

(defvar claude-idle-notify-app
  (expand-file-name "~/Applications/Claude (Emacs).app")
  "A terminal-notifier clone whose bundle name is shown on notifications.
macOS takes the app name and icon from the sending bundle, so plain
`terminal-notifier' notifications look like they come from a terminal.
Create the clone with bin/make-claude-notifier-app; if it is missing,
`claude-idle-notify--darwin' falls back to `terminal-notifier' on PATH.")

(defun claude-idle-notify--notifier ()
  "Path to the terminal-notifier binary to send with, or nil if none."
  (let ((cloned (expand-file-name "Contents/MacOS/terminal-notifier"
                                  claude-idle-notify-app)))
    (or (and (file-executable-p cloned) cloned)
        (executable-find "terminal-notifier"))))

(defun claude-idle-notify--linux (persp buffer-id buffer-name message)
  "Show a D-Bus desktop notification with a jump action."
  (notifications-notify
   :title (claude-idle-notify--title persp buffer-name)
   :body message
   :actions '("jump" "Go to buffer")
   :on-action (lambda (_id _key) (claude-idle-jump persp buffer-id))))

(defun claude-idle-notify--darwin (persp buffer-id buffer-name message)
  "Show a macOS notification, using terminal-notifier's -execute for the
jump action when available, else a plain osascript notification."
  (let ((notifier (claude-idle-notify--notifier))
        (title (claude-idle-notify--title persp buffer-name)))
    (if notifier
        (call-process notifier nil 0 nil
                      "-title" title
                      "-message" message
                      "-execute" (format "%s --eval %s"
                                         (shell-quote-argument llm-emacsclient-program)
                                         (shell-quote-argument
                                          (format "(claude-idle-jump %S %S)" persp buffer-id))))
      (call-process "osascript" nil 0 nil "-e"
                    (format "display notification %s with title %s"
                            (prin1-to-string message)
                            (prin1-to-string title))))))

(defun claude-idle-notify (persp buffer-id message)
  "Record and surface an idle notification for PERSP/BUFFER-ID with MESSAGE.
Called via emacsclient from bin/claude-idle-notify (a Claude Code
`Notification' hook)."
  (let* ((buffer (notify--find-buffer-by-id buffer-id))
         (buffer-name (if buffer (buffer-name buffer) buffer-id)))
    (push (list :time (current-time) :persp persp :buffer-id buffer-id
                :buffer-name buffer-name :message message)
          claude-idle-notifications)
    (when (> (length claude-idle-notifications) claude-idle-notifications-max)
      (setq claude-idle-notifications
            (seq-take claude-idle-notifications claude-idle-notifications-max)))
    (funcall (claude-idle-notify--backend) persp buffer-id buffer-name message)))

(defun claude-idle-notifications-prune ()
  "Drop entries from `claude-idle-notifications' whose buffer no longer exists."
  (setq claude-idle-notifications
        (cl-remove-if-not (lambda (entry)
                             (notify--find-buffer-by-id (plist-get entry :buffer-id)))
                           claude-idle-notifications)))

(defun claude-idle-notifications-clear ()
  "Drop every entry from `claude-idle-notifications'."
  (interactive)
  (let ((n (length claude-idle-notifications)))
    (setq claude-idle-notifications nil)
    (message "Cleared %d idle notification%s" n (if (= n 1) "" "s"))))

(defun claude-idle-notifications-clear-and-exit ()
  "Clear all idle notifications and quit the minibuffer."
  (interactive)
  (claude-idle-notifications-clear)
  (abort-recursive-edit))

(defvar claude-idle-notifications-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'claude-idle-notifications-clear-and-exit)
    map)
  "Extra keys active while picking an idle notification.")

(defun claude-idle-notifications--setup-minibuffer ()
  "Layer `claude-idle-notifications-minibuffer-map' over the minibuffer map."
  (use-local-map (make-composed-keymap
                  claude-idle-notifications-minibuffer-map
                  (current-local-map))))

(defun claude-idle-notifications-list ()
  "Pick a recent idle notification and jump to its perspective/buffer.
Press \\<claude-idle-notifications-minibuffer-map>\\[claude-idle-notifications-clear-and-exit] \
in the minibuffer to drop all pending notifications."
  (interactive)
  (claude-idle-notifications-prune)
  (if (null claude-idle-notifications)
      (message "No recent idle notifications")
    (let* ((choices
            (mapcar (lambda (entry)
                      (cons (format "[%s] %s — %s: %s"
                                    (format-time-string "%H:%M:%S" (plist-get entry :time))
                                    (plist-get entry :persp)
                                    (plist-get entry :buffer-name)
                                    (plist-get entry :message))
                            entry))
                    claude-idle-notifications))
           (pick (minibuffer-with-setup-hook
                     #'claude-idle-notifications--setup-minibuffer
                   (completing-read "Jump to idle agent (C-c C-k clears all): "
                                    choices nil t)))
           (entry (cdr (assoc pick choices))))
      (when entry
        (setq claude-idle-notifications (delq entry claude-idle-notifications))
        (claude-idle-jump (plist-get entry :persp) (plist-get entry :buffer-id))))))

(global-set-key (kbd "C-c n") #'claude-idle-notifications-list)
(global-set-key (kbd "C-c N") #'claude-idle-notifications-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Launching Claude with an AWS_PROFILE override, for MCP servers that need
;; AWS credentials resolved via a specific profile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun claude-code-ide--read-aws-profile ()
  "Prompt for an AWS profile, completing over ~/.aws/config profiles."
  (let (profiles)
    (when (file-exists-p "~/.aws/config")
      (with-temp-buffer
        (insert-file-contents (expand-file-name "~/.aws/config"))
        (goto-char (point-min))
        (while (re-search-forward "^\\[profile \\(.+\\)\\]" nil t)
          (push (match-string 1) profiles))))
    (completing-read "AWS profile: " (nreverse profiles) nil nil (getenv "AWS_PROFILE"))))

(defun claude-code-ide-aws-profile (profile &optional relaunch)
  "Start Claude Code for the current project with AWS_PROFILE set to PROFILE.
With a prefix argument (RELAUNCH), stop the existing session for this
directory first and resume its most recent conversation under the new
profile instead of starting a fresh session."
  (interactive (list (claude-code-ide--read-aws-profile) current-prefix-arg))
  (when relaunch
    (claude-code-ide-stop))
  (let ((process-environment (cons (format "AWS_PROFILE=%s" profile) process-environment)))
    (if relaunch
        (claude-code-ide-resume)
      (claude-code-ide))))

(with-eval-after-load 'claude-code-ide-transient
	(transient-append-suffix 'claude-code-ide-menu "r"
		'("a" "Start with AWS profile (C-u: relaunch)" claude-code-ide-aws-profile)))

(provide 'llm)
;;; llm.el ends here
