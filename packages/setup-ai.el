;;; setup-ai.el --- AI tooling configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-related packages.
;;
;; Sections:
;;   0. agent protocol — standardized keys across all harnesses
;;   1. eca            — editor-code-assistant/eca-emacs
;;   2. claude-code-ide — manzaltu/claude-code-ide.el (parsnips' ghostel fork)
;;   3. agent-shell    — xenodium/agent-shell, ACP-based
;;   4. pi-coding-agent — dnouri/pi-coding-agent
;;
;; Standard keys (active globally when `agent-mode' is on):
;;   C-c e   toggle the current harness's chat window (and focus it)
;;   C-c i   send region (or prompted text) to the current harness
;;   C-c .   open the current harness's menu
;;   C-c ,   switch the global harness default
;;
;; Standard in-buffer keys (active in every harness's chat/input buffer):
;;   C-c .     open menu
;;   C-c C-k   interrupt / abort generation
;;   C-c C-n   new session
;;   C-c C-f   pick / switch session
;;
;; Queueing a message (send-when-idle semantics) is available via the harness
;; transient menu (`C-c .` then RET in agent-shell).
;;
;; "The current harness" is resolved by `agent--active-backend' in three
;; tiers:
;;   1. if point is inside a harness chat buffer, that harness wins;
;;   2. else the value of `agent-current' (settable per-project via
;;      .dir-locals.el thanks to its :safe predicate);
;;   3. else the defcustom default.

;;; Code:

(require 'transient)
(require 'package)
(require 'buffers)

;; Forward declarations — the underlying packages are loaded lazily via
;; use-package, so the dispatcher references them before they are defined.
(declare-function eca-chat-toggle-window "eca-chat")
(declare-function eca-chat-add-context-to-user-prompt "eca-chat")
(declare-function eca-chat-send-prompt "eca-chat")
(declare-function eca-chat-add-filepath-to-user-prompt "eca-chat")
(declare-function eca-transient-menu "eca")
(declare-function eca-chat-stop-prompt "eca-chat")
(declare-function eca-chat-new "eca-chat")
(declare-function eca-chat-select "eca-chat")

(declare-function agent-shell "agent-shell")
(declare-function agent-shell-menu "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-new-shell "agent-shell")
(declare-function agent-shell-other-buffer "agent-shell")
(declare-function agent-shell-send-region "agent-shell" (&optional pick-shell))
(declare-function agent-shell-send-file "agent-shell" (&optional prompt-for-file pick-shell))
(declare-function agent-shell-queue-request "agent-shell")
(declare-function pi-coding-agent-queue-steering "pi-coding-agent")
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")

(declare-function claude-code-ide "claude-code-ide")
(declare-function claude-code-ide-emacs-tools-setup "claude-code-ide-emacs-tools")
(declare-function claude-code-ide-toggle "claude-code-ide")
(declare-function claude-code-ide-menu "claude-code-ide")
(declare-function claude-code-ide-send-escape "claude-code-ide")
(declare-function claude-code-ide-send-prompt "claude-code-ide")
(declare-function claude-code-ide-insert-at-mentioned "claude-code-ide")
(declare-function claude-code-ide-list-sessions "claude-code-ide")

(declare-function pi-coding-agent "pi-coding-agent")
(declare-function pi-coding-agent-menu "pi-coding-agent")
(declare-function pi-coding-agent-abort "pi-coding-agent")
(declare-function pi-coding-agent-new-session "pi-coding-agent")
(declare-function pi-coding-agent-resume-session "pi-coding-agent")

;; Variables referenced before their defining package loads (silence the
;; byte-compiler's free-variable warnings; the real definitions live in eca).
(defvar eca-chat--context-completion-cache)
(defvar eca-chat--file-completion-cache)

;; ─────────────────────────────────────────────────────────────────────────────
;; 0. Agent protocol
;; ─────────────────────────────────────────────────────────────────────────────

(defgroup agent nil
  "Unified keymap and dispatch across AI coding-agent harnesses."
  :group 'tools
  :prefix "agent-")

(defconst agent-backends '(eca agent-shell pi claude-code-ide)
  "Known agent backends, in display order.")

(defcustom agent-current 'eca
  "Which agent harness the standard keys target by default.
Override per-project via `.dir-locals.el':
  ((nil . ((agent-current . eca))))"
  :type '(choice (const eca)
                 (const agent-shell)
                 (const pi)
                 (const claude-code-ide))
  :safe (lambda (v) (memq v agent-backends))
  :group 'agent)

(defvar-local agent--claude-buffer-p nil
  "Non-nil in buffers created by claude-code-ide.
Set by an after-advice on `claude-code-ide' because the package does not
define its own major mode — the buffer inherits from vterm/eat/ghostel.")

(defun agent--active-backend ()
  "Resolve the agent backend for the current buffer.
Inside a harness chat/input buffer the backend is inferred from the major
mode; elsewhere the value of `agent-current' is used."
  (cond
   ((derived-mode-p 'eca-chat-mode) 'eca)
   ((derived-mode-p 'agent-shell-mode) 'agent-shell)
   ((derived-mode-p 'pi-coding-agent-chat-mode
                    'pi-coding-agent-input-mode) 'pi)
   (agent--claude-buffer-p 'claude-code-ide)
   (t agent-current)))

(defun agent--label (backend)
  "Return a human-readable label for BACKEND."
  (pcase backend
    ('eca             "ECA")
    ('agent-shell     "agent-shell")
    ('pi              "pi")
    ('claude-code-ide "Claude Code IDE")
    (_ (symbol-name backend))))

(defun agent-toggle-window ()
  "Toggle the chat window of the current agent backend and focus it."
  (interactive)
  (pcase (agent--active-backend)
    ('eca             (call-interactively #'my/eca-toggle))
    ('agent-shell     (call-interactively #'my/agent-shell-toggle))
    ('pi              (call-interactively #'my/pi-coding-agent-toggle))
    ('claude-code-ide (call-interactively #'claude-code-ide-toggle))
    (backend (user-error "Unknown agent backend: %s" backend))))

(defun agent-send-chat-context-dwim (beg end)
  "Send context to the current agent in a DWIM manner.
BEG and END bound the active region, or are nil when no region is active.
They are resolved in the `interactive' form rather than via the \"r\" spec so
the command never errors with \"The mark is not set\" in buffers that have
never had a mark.
Resolution order:
  1. With an active region BEG..END, delegate to the backend's region-sender
     (which is expected to toggle/focus its chat window).
  2. With no region but visiting a file, send a reference to that file.
     Most harnesses accept an @path/to/file mention; each backend's native
     file-sender handles the current buffer's file.
  3. Otherwise (a non-file buffer), read free-form text from the minibuffer
     and send it as a prompt."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((backend (agent--active-backend)))
    (cond
     ;; 1. Region selected → send the region.
     ((use-region-p)
      (pcase backend
        ('eca             (call-interactively #'eca-chat-add-context-to-user-prompt))
        ('agent-shell     (call-interactively #'agent-shell-send-region))
        ('pi              (my/pi-send-dwim beg end))
        ('claude-code-ide (call-interactively #'claude-code-ide-insert-at-mentioned))
        (_ (user-error "Unknown agent backend: %s" backend))))
     ;; 2. No region, but visiting a file → send the file itself.
     ((buffer-file-name)
      (pcase backend
        ('eca             (call-interactively #'eca-chat-add-filepath-to-user-prompt))
        ('agent-shell     (call-interactively #'agent-shell-send-file))
        ('pi              (my/pi-send-file))
        ('claude-code-ide (call-interactively #'claude-code-ide-insert-at-mentioned))
        (_ (user-error "Unknown agent backend: %s" backend))))
     ;; 3. Non-file buffer → prompt for free-form text.
     (t
      (let ((text (read-string (format "Send to %s: " (agent--label backend)))))
        (unless (string-empty-p text)
          (pcase backend
            ('eca             (eca-chat-send-prompt text))
            ('agent-shell     (progn (my/agent-shell-toggle)
                                     (insert text)))
            ('pi              (my/pi-send-to-input (concat text "\n")))
            ('claude-code-ide (claude-code-ide-send-prompt text))
            (_ (user-error "Unknown agent backend: %s" backend)))))))))

(defun agent-open-menu ()
  "Open the transient menu of the current agent backend."
  (interactive)
  (pcase (agent--active-backend)
    ('eca             (call-interactively #'eca-transient-menu))
    ('agent-shell     (call-interactively #'agent-shell-menu))
    ('pi              (call-interactively #'pi-coding-agent-menu))
    ('claude-code-ide (call-interactively #'claude-code-ide-menu))
    (backend (user-error "Unknown agent backend: %s" backend))))

(defun agent-interrupt ()
  "Stop the in-progress generation for the current agent backend."
  (interactive)
  (pcase (agent--active-backend)
    ('eca             (call-interactively #'eca-chat-stop-prompt))
    ('agent-shell     (call-interactively #'agent-shell-interrupt))
    ('pi              (call-interactively #'pi-coding-agent-abort))
    ('claude-code-ide (call-interactively #'claude-code-ide-send-escape))
    (backend (user-error "Unknown agent backend: %s" backend))))

(defun agent-new-session ()
  "Start a new session with the current agent backend."
  (interactive)
  (pcase (agent--active-backend)
    ('eca             (call-interactively #'eca-chat-new))
    ('agent-shell     (call-interactively #'agent-shell-new-shell))
    ('pi              (call-interactively #'pi-coding-agent-new-session))
    ('claude-code-ide (call-interactively #'claude-code-ide))
    (backend (user-error "Unknown agent backend: %s" backend))))

(defun agent-switch-session ()
  "Pick among existing sessions of the current agent backend."
  (interactive)
  (pcase (agent--active-backend)
    ('eca             (call-interactively #'eca-chat-select))
    ('agent-shell     (call-interactively #'agent-shell-other-buffer))
    ('pi              (call-interactively #'pi-coding-agent-resume-session))
    ('claude-code-ide (call-interactively #'claude-code-ide-list-sessions))
    (backend (user-error "Unknown agent backend: %s" backend))))

(defun agent-queue-message ()
  "Queue a message to the current agent backend (send now if idle).
Queueing is only supported by agent-shell and pi; ECA and claude-code-ide
do not expose a queue API."
  (interactive)
  (pcase (agent--active-backend)
    ('agent-shell     (call-interactively #'agent-shell-queue-request))
    ('pi              (call-interactively #'pi-coding-agent-queue-steering))
    ('eca             (user-error "Queue not supported for ECA"))
    ('claude-code-ide (user-error "Queue not supported for Claude Code IDE"))
    (backend          (user-error "Queue not supported for backend: %s" backend))))

(defun agent-switch-backend ()
  "Interactively set `agent-current' for this Emacs session."
  (interactive)
  (let* ((choice (completing-read
                  "Set default agent backend: "
                  (mapcar #'symbol-name agent-backends)
                  nil t nil nil
                  (symbol-name agent-current))))
    (setq agent-current (intern choice))
    (message "Agent backend: %s" (agent--label agent-current))))

(defvar agent-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .")   #'agent-open-menu)
    (define-key map (kbd "C-c C-k") #'agent-interrupt)
    (define-key map (kbd "C-c C-n") #'agent-new-session)
    (define-key map (kbd "C-c C-f") #'agent-switch-session)
    map)
  "Keymap for `agent-chat-mode', active inside harness chat/input buffers.")

(define-minor-mode agent-chat-mode
  "Minor mode installed in every agent harness chat/input buffer.
Provides a consistent set of keybindings across eca, agent-shell,
pi-coding-agent, and claude-code-ide buffers."
  :lighter " Agent"
  :keymap agent-chat-mode-map)

(defvar agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") #'agent-toggle-window)
    (define-key map (kbd "C-c i") #'agent-send-chat-context-dwim)
    (define-key map (kbd "C-c .") #'agent-open-menu)
    (define-key map (kbd "C-c ,") #'agent-switch-backend)
    map)
  "Global keymap for `agent-mode'.")

(define-minor-mode agent-mode
  "Global minor mode installing the unified agent-harness keybindings."
  :global t
  :lighter nil
  :keymap agent-mode-map)

(agent-mode 1)

;; ─────────────────────────────────────────────────────────────────────────────
;; 1. eca — Editor Code Assistant
;; ─────────────────────────────────────────────────────────────────────────────

(defun my/eca-fix-autoloads ()
  "Regenerate ECA autoloads if broken by package-vc.
package-vc sometimes produces an empty autoloads file with no actual
autoload forms — just the `load-path' boilerplate.  Detect by absence of
any `(autoload' form and regenerate if needed."
  (when-let* ((pkg-dir (and (package-installed-p 'eca)
                            (package-desc-dir (cadr (assq 'eca package-alist)))))
              (autoloads-file (expand-file-name "eca-autoloads.el" pkg-dir)))
    (when (and (file-exists-p autoloads-file)
               (with-temp-buffer
                 (insert-file-contents autoloads-file)
                 (not (search-forward "(autoload " nil t))))
      (message "ECA: Regenerating missing autoloads...")
      (package-generate-autoloads "eca" pkg-dir)
      (load autoloads-file nil t)
      (message "ECA: Autoloads regenerated successfully."))))

(defun my/eca-recover-completion-item (item)
  "Return ITEM with `eca-chat-completion-item' text property restored if missing.
Some completion UIs (e.g. `consult-completion-in-region') call `completing-read'
and insert the returned string without text properties.  This searches the
buffer-local completion caches for a candidate whose plain text matches ITEM
and returns the original propertized string."
  (if (get-text-property 0 'eca-chat-completion-item item)
      item
    (let ((found nil))
      (dolist (cache (list eca-chat--context-completion-cache
                           eca-chat--file-completion-cache))
        (unless found
          (maphash (lambda (_query candidates)
                     (unless found
                       (setq found (seq-find
                                    (lambda (c)
                                      (string= (substring-no-properties c) item))
                                    candidates))))
                   cache)))
      (or found item))))

(defun my/eca-chat-mode-hook ()
  "Disable various minor modes in ECA chat buffers for cleaner experience."
  (when (fboundp 'denote-rename-buffer-mode) (denote-rename-buffer-mode -1)))

(my/eca-fix-autoloads)

(use-package eca
  :defines (eca-chat-focus-on-open
            eca-chat-mode-map)
  :functions (eca-session
              eca-chat--get-last-buffer
              eca-chat--display-buffer
              eca-chat-send-prompt
              eca-chat--expandable-content-toggle
              eca-chat--add-expandable-content)
  :preface
  (defun my/eca-send-prompt-from-minibuffer ()
    "Prompt for a message in the minibuffer and send it to the current ECA chat."
    (interactive)
    (let ((prompt (read-string "ECA Prompt: ")))
      (when (and prompt (not (string-empty-p prompt)))
        (eca-chat-send-prompt prompt))))

  (defun my/eca--display-buffer (buffer &optional focus)
    "Display ECA chat BUFFER as a dismissible right side window.
Unlike `eca-chat--display-buffer', the window does NOT carry the
`no-delete-other-windows' parameter, so `C-x 1' and full-screen
commands (magit, etc.) can close it normally.
When FOCUS is non-nil, select the window after displaying it."
    (let* ((action '((display-buffer-in-side-window)
                     (side . right)
                     (slot . 0)
                     (window-width . 0.40)))
           (win (display-buffer buffer action)))
      (when (and win focus)
        (select-window win))
      win))

  (defun my/eca-toggle ()
    "Toggle the ECA chat window as a dismissible right side window.
Mirrors the behaviour of `my/agent-shell-toggle': visible window is
closed with `delete-window', invisible buffer is shown via
`my/eca--display-buffer' (no `no-delete-other-windows' guard)."
    (interactive)
    (when-let* ((session (eca-session)))
      (let ((buffer (eca-chat--get-last-buffer session)))
        (if (buffer-live-p buffer)
            (if-let ((win (get-buffer-window buffer)))
                ;; Window is visible — close it (just like agent-shell-toggle)
                (if (> (count-windows) 1)
                    (delete-window win)
                  (switch-to-prev-buffer))
              ;; Window is hidden — show it and focus it
              (my/eca--display-buffer buffer t)
              (with-current-buffer buffer
                (goto-char (point-max))))
          ;; No buffer yet — fall back to the standard open command
          (eca-chat-toggle-window)))))

  (defun my/eca-ensure-chat-window-visible (&rest _)
    "Ensure the ECA chat window is visible before interacting with it."
    (when-let* ((session (eca-session))
                (buffer (eca-chat--get-last-buffer session)))
      (unless (get-buffer-window buffer t)
        (my/eca--display-buffer buffer nil))))

  (defvar-local my/eca-auto-expand-blocks nil
    "When non-nil, auto-expand new expandable blocks as they are added.")

  (defun my/eca-auto-expand-block (id &rest _)
    "Expand ID immediately if `my/eca-auto-expand-blocks' is set in this buffer."
    (when (and (derived-mode-p 'eca-chat-mode) my/eca-auto-expand-blocks)
      (eca-chat--expandable-content-toggle id t nil)))

  (defun my/eca-chat-expand-all-blocks (&optional sticky)
    "Expand all collapsed blocks in the current ECA chat buffer.
With prefix argument STICKY, also keep all future blocks in this
buffer expanded automatically."
    (interactive "P")
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id)))
        (eca-chat--expandable-content-toggle id t nil)))
    (when sticky
      (setq-local my/eca-auto-expand-blocks t)
      (message "ECA: future blocks in this buffer will auto-expand")))

  (defun my/eca-chat-collapse-all-blocks (&optional sticky)
    "Collapse all expanded blocks in the current ECA chat buffer.
With prefix argument STICKY, also disable future auto-expansion in
this buffer."
    (interactive "P")
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id)))
        (eca-chat--expandable-content-toggle id t t)))
    (when sticky
      (setq-local my/eca-auto-expand-blocks nil)
      (message "ECA: auto-expand disabled for this buffer")))

  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :hook ((eca-chat-mode . my/eca-chat-mode-hook)
         (eca-chat-mode . agent-chat-mode))
  :bind ((:map eca-chat-mode-map
               ("C-c C-o" . my/eca-chat-expand-all-blocks)
               ("C-c C-c" . my/eca-chat-collapse-all-blocks)))
  :ensure t
  :config
  ;; Ensure chat window is visible before adding context.  Both senders end by
  ;; calling `eca-chat--select-window', which errors when the window is hidden,
  ;; so display it first.
  (advice-add 'eca-chat-add-context-to-user-prompt :before #'my/eca-ensure-chat-window-visible)
  (advice-add 'eca-chat-add-filepath-to-user-prompt :before #'my/eca-ensure-chat-window-visible)

  ;; Register the auto-expand advice once globally — it is a no-op in buffers
  ;; where my/eca-auto-expand-blocks is nil (the default).
  (advice-add 'eca-chat--add-expandable-content :after #'my/eca-auto-expand-block)

  ;; Fix @file completion with consult-completion-in-region (and any completing-read-based
  ;; UI).  Those UIs call completing-read and insert the returned string, which strips text
  ;; properties — including `eca-chat-completion-item' that exit functions need to resolve
  ;; the selected candidate.  The advice recovers the propertized original from the cache.
  (advice-add 'eca-chat--completion-context-from-prompt-exit-function
              :filter-args (lambda (args)
                             (cons (my/eca-recover-completion-item (car args)) (cdr args))))
  (advice-add 'eca-chat--completion-context-from-new-context-exit-function
              :filter-args (lambda (args)
                             (cons (my/eca-recover-completion-item (car args)) (cdr args))))
  (advice-add 'eca-chat--completion-file-from-prompt-exit-function
              :filter-args (lambda (args)
                             (cons (my/eca-recover-completion-item (car args)) (cdr args))))

  (setq eca-chat-hide-markdown-markup nil)

  :custom
  (eca-chat-auto-add-repomap nil)
  ;; (eca-worktree-mode 'isolated)
  ;; Keep tool call blocks open after they complete — no manual tabbing needed.
  (eca-chat-shrink-called-tools nil)
  ;; Disable ECA's built-in side-window logic so it never attaches
  ;; `no-delete-other-windows' to the chat window.  All display is
  ;; handled by `my/eca--display-buffer' / `my/eca-toggle', which
  ;; mirrors agent-shell's dismissible-side-window behaviour.
  (eca-chat-use-side-window nil))

;; ─────────────────────────────────────────────────────────────────────────────
;; 2. claude-code-ide
;;    Using parsnips' fork with ghostel backend support (PR #190 upstream).
;;    Once merged upstream, switch back to:
;;      :vc (:url "https://github.com/manzaltu/claude-code-ide.el")
;; ─────────────────────────────────────────────────────────────────────────────

(defun my/claude-code-ide--mark-buffer (&rest _)
  "Mark the current buffer as a claude-code-ide buffer and enable agent keys."
  (setq-local agent--claude-buffer-p t)
  (agent-chat-mode 1))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :custom
  ;; Window configuration
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 90)
  (claude-code-ide-window-height 20)
  (claude-code-ide-use-side-window t)
  (claude-code-ide-focus-on-open t)
  (claude-code-ide-focus-claude-after-ediff t)
  (claude-code-ide-show-claude-window-in-ediff t)

  ;; CLI configuration
  (claude-code-ide-cli-path "claude")
  (claude-code-ide-cli-debug nil)
  (claude-code-ide-cli-extra-flags "")
  (claude-code-ide-system-prompt nil)

  ;; MCP tools
  (claude-code-ide-mcp-allowed-tools 'auto)

  ;; Terminal backend — set to 'ghostel to test, 'vterm is the default
  (claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-prevent-reflow-glitch t)
  :config
  (claude-code-ide-emacs-tools-setup)
  ;; After claude-code-ide creates its session buffer, mark it and enable
  ;; the unified in-buffer keymap.  The package does not define its own major
  ;; mode, so we cannot hook on that — we rely on the command creating the
  ;; buffer and leaving it current.
  (advice-add 'claude-code-ide :after #'my/claude-code-ide--mark-buffer))

;; ─────────────────────────────────────────────────────────────────────────────
;; 3. agent-shell
;; ─────────────────────────────────────────────────────────────────────────────

(defun my/agent-shell-toggle ()
  "Toggle agent shell display, falling back to any shell if none for project."
  (interactive)
  (if-let ((shell-buffer (or (and (derived-mode-p 'agent-shell-mode)
                                  (current-buffer))
                             (seq-first (agent-shell-project-buffers))
                             (seq-first (agent-shell-buffers)))))
      (if-let ((window (get-buffer-window shell-buffer)))
          (if (> (count-windows) 1)
              (delete-window window)
            (switch-to-prev-buffer))
        (agent-shell--display-buffer shell-buffer))
    (call-interactively #'agent-shell)))

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell" :rev :newest)
  :init
  (require 'diff)
  :hook (agent-shell-mode . agent-chat-mode)
  :custom
  ;; Window configuration
  (agent-shell-display-action
   '((display-buffer-in-side-window)
     (side . right)
     (window-width . 90)))
  ;; Show icons for different agent configs
  (agent-shell-show-config-icons t)
  ;; Expand the thought-process blocks but collapse tool-use blocks by default
  (agent-shell-thought-process-expand-by-default t)
  (agent-shell-tool-use-expand-by-default nil)
  ;; Context sources for prompts
  (agent-shell-context-sources '(files region error line))
  :config
  (transient-define-prefix agent-shell-menu ()
    "Agent Shell commands."
    ["Shell"
     [("a" "Open shell" agent-shell)
      ("t" "Toggle shell" agent-shell-toggle)
      ("n" "New shell" agent-shell-new-shell)
      ("S" "Resume session" agent-shell-resume-session)
      ("c" "Compose prompt" agent-shell-prompt-compose)
      ("o" "Other buffer" agent-shell-other-buffer)]
     [("i" "Interrupt" agent-shell-interrupt)
      ("r" "Resume pending" agent-shell-resume-pending-requests)
      ("R" "Remove pending" agent-shell-remove-pending-request)]]
    ["Session"
     [("m" "Set model" agent-shell-set-session-model)
      ("M" "Set mode" agent-shell-set-session-mode)
      ("C" "Cycle mode" agent-shell-cycle-session-mode)]]
    ["Send"
     [("RET" "Queue request" agent-shell-queue-request)
      ("f" "Send file" agent-shell-send-file)
      ("F" "Send other file" agent-shell-send-other-file)
      ("s" "Send screenshot" agent-shell-send-screenshot)
      ("!" "Insert shell output" agent-shell-insert-shell-command-output)]]
    ["Logs & Debug"
     [("T" "Open transcript" agent-shell-open-transcript)
      ("v" "View traffic" agent-shell-view-traffic)
      ("l" "View ACP logs" agent-shell-view-acp-logs)
      ("L" "Toggle logging" agent-shell-toggle-logging)
      ("V" "Version" agent-shell-version)]]))

;; ─────────────────────────────────────────────────────────────────────────────
;; 4. pi-coding-agent — Emacs frontend for the pi CLI coding agent
;;    https://github.com/dnouri/pi-coding-agent
;;
;; REQUIREMENTS
;;   - Emacs 29+ with tree-sitter support
;;   - pi CLI installed and authenticated:
;;       npm install -g @mariozechner/pi-coding-agent
;;       pi --login
;;
;; SIDE PANEL DESIGN
;;   Uses `display-buffer-in-side-window' so the panel lives at the right
;;   edge of the frame independently of your main windows.  C-x 1 in the
;;   main area never kills it.  Width is controlled by `my/pi-panel-width'
;;   (default 0.4 = 40% of the frame).
;;
;;   Mode-line tweaks applied when the panel opens:
;;     - Chat buffer: mode-line hidden entirely (input below gives context).
;;     - Input buffer: shortened to *pi:<project>* and auto-sudoedit-mode
;;       (ASE) indicator suppressed.
;;
;; HELPER FUNCTIONS (reusable building blocks)
;;   `my/pi-send-to-input' TEXT
;;       Appends TEXT to the pi input buffer for the current project,
;;       opens the side panel if hidden, and focuses the input window.
;;       Use this to build your own send commands (e.g. send a compile
;;       error, a git diff hunk, a CIDER exception, etc.).
;; ─────────────────────────────────────────────────────────────────────────────

(use-package pi-coding-agent
  :vc (:url "https://github.com/dnouri/pi-coding-agent" :rev :newest)

  :functions (pi-coding-agent--session-directory
              pi-coding-agent--find-session
              pi-coding-agent--setup-session)

  :preface

  (defcustom my/pi-panel-width 0.4
    "Width of the pi side panel as a fraction of the frame width."
    :type 'number
    :group 'pi-coding-agent)

  (defun my/pi--short-name (dir)
    "Return the last path component of DIR as a short project name."
    (file-name-nondirectory (directory-file-name dir)))

  (defun my/pi--open-panel (chat-buf input-buf dir)
    "Display CHAT-BUF (top) and INPUT-BUF (bottom) as a right side panel.
DIR is the session directory used to derive the short project name shown
in the mode-line.  Uses side windows so the main window layout is untouched."
    (let ((input-height (or (bound-and-true-p pi-coding-agent-input-window-height) 10))
          (win-params   '((no-delete-other-windows . t)))
          (label        (format " *pi:%s*" (my/pi--short-name dir))))
      (display-buffer chat-buf
                      `((display-buffer-in-side-window)
                        (side              . right)
                        (slot              . 0)
                        (window-width      . ,my/pi-panel-width)
                        (window-parameters . ,win-params)))
      (display-buffer input-buf
                      `((display-buffer-in-side-window)
                        (side              . right)
                        (slot              . 1)
                        (window-width      . ,my/pi-panel-width)
                        (window-height     . ,input-height)
                        (window-parameters . ,win-params)))
      ;; Chat: hide mode-line entirely (input below provides context)
      (with-current-buffer chat-buf
        (setq-local mode-line-format nil))
      ;; Input: short label + suppress ASE indicator
      (with-current-buffer input-buf
        (setq-local mode-line-buffer-identification
                    (list (propertize label 'face 'mode-line-buffer-id)))
        (make-local-variable 'minor-mode-alist)
        (setq minor-mode-alist
              (assq-delete-all 'auto-sudoedit-mode minor-mode-alist)))))

  (defun my/pi--close-panel (chat-buf input-buf)
    "Close the pi side panel windows for CHAT-BUF and INPUT-BUF."
    (dolist (buf (list chat-buf input-buf))
      (when-let ((win (get-buffer-window buf)))
        (delete-window win))))

  (defun my/pi-coding-agent-toggle ()
    "Toggle the pi side panel for the current project.
Opens a right-side panel with chat on top and input on bottom.
Creates a new session if none exists for the current project."
    (interactive)
    (let* ((dir       (pi-coding-agent--session-directory))
           (chat-buf  (pi-coding-agent--find-session dir))
           (input-buf (and chat-buf
                           (buffer-local-value
                            'pi-coding-agent--input-buffer chat-buf))))
      (if (and chat-buf (get-buffer-window chat-buf))
          (my/pi--close-panel chat-buf input-buf)
        (unless chat-buf
          (setq chat-buf  (pi-coding-agent--setup-session dir))
          (setq input-buf (buffer-local-value
                           'pi-coding-agent--input-buffer chat-buf)))
        (my/pi--open-panel chat-buf input-buf dir)
        (when-let ((win (get-buffer-window input-buf)))
          (select-window win)))))

  (defun my/pi-send-to-input (text)
    "Append TEXT to the pi input buffer for the current project and focus it.
Creates a new pi session if one does not exist yet.
Opens the side panel if not currently visible."
    (let* ((dir      (pi-coding-agent--session-directory))
           (chat-buf (or (pi-coding-agent--find-session dir)
                         (pi-coding-agent--setup-session dir)))
           (input-buf (buffer-local-value 'pi-coding-agent--input-buffer chat-buf)))
      (with-current-buffer input-buf
        (goto-char (point-max))
        (unless (bobp)
          (unless (bolp) (insert "\n")))
        (insert text))
      (unless (get-buffer-window chat-buf)
        (my/pi--open-panel chat-buf input-buf dir))
      ;; Scroll chat to bottom so window-point-insertion-type keeps it there
      ;; as pi streams its response.
      (when-let ((chat-win (get-buffer-window chat-buf)))
        (set-window-point chat-win (with-current-buffer chat-buf (point-max))))
      (when-let ((win (get-buffer-window input-buf)))
        (select-window win))
      (goto-char (point-max))))

  ;; ── Image paste support ──────────────────────────────────────────────────
  ;; When in the pi input buffer and the clipboard holds an image, C-y saves
  ;; it to a temp file and inserts the path so pi picks it up as a file
  ;; reference.  Plain text yanks are unaffected.
  ;; Mirrors the approach used by eca (eca-chat-context.el).

  (defun my/pi--yank-image-handler (type data)
    "Save clipboard image DATA of MIME TYPE to a temp file and insert path."
    (let* ((ext  (if (string-match "image/\\(.*\\)" (symbol-name type))
                     (match-string 1 (symbol-name type))
                   "png"))
           (path (make-temp-file "pi-clipboard-picture-" nil (concat "." ext))))
      (let ((coding-system-for-write 'no-conversion))
        (write-region data nil path nil 'silent))
      (insert path " ")
      (message "Image saved to %s" path)))

  (defun my/pi--clipboard-has-image-p ()
    "Return non-nil if the clipboard contains image data."
    (when-let* ((targets (ignore-errors
                           (gui-get-selection 'CLIPBOARD 'TARGETS))))
      (seq-some (lambda (type)
                  (and (symbolp type)
                       (string-match-p "^image/" (symbol-name type))))
                (if (vectorp targets) (append targets nil) targets))))

  (defun my/pi--yank-considering-image (orig-fun &rest args)
    "Yank image from clipboard when in pi input buffer, else call ORIG-FUN."
    (if (and (display-graphic-p)
             (derived-mode-p 'pi-coding-agent-input-mode)
             (fboundp 'yank-media)
             (boundp 'yank-media--registered-handlers)
             yank-media--registered-handlers
             (my/pi--clipboard-has-image-p))
        (call-interactively #'yank-media)
      (apply orig-fun args)))

  (defun my/pi--setup-image-yank ()
    "Register image yank handlers for the current pi input buffer."
    (when (fboundp 'yank-media-handler)
      (setq-local yank-media--registered-handlers nil)
      (dolist (mime '("image/png" "image/jpeg" "image/jpg"
                      "image/gif" "image/webp"))
        (yank-media-handler mime #'my/pi--yank-image-handler)))
    (advice-add 'yank :around #'my/pi--yank-considering-image))

  (defun my/pi-send-dwim (beg end)
    "Send to pi: region as a fenced code block, or prompt for free-form text.
BEG and END bound the active region, or are nil when no region is active.
With an active region, formats it with file path and line range and sends it.
Without a region, reads a message from the minibuffer and sends that."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil)))
    (if (use-region-p)
        (my/pi-send-to-input (region-to-string beg end))
      (let ((text (read-string "Send to pi: ")))
        (unless (string-empty-p text)
          (my/pi-send-to-input (concat text "\n"))))))

  (defun my/pi-send-file ()
    "Send a reference to the current buffer's file to the pi session.
Unlike the other harnesses, pi exposes no one-shot send-file command — only
interactive `@' completion in its input buffer.  This inserts pi's native
@path file reference (project-relative when possible) so pi resolves it as a
file, mirroring what `@' completion produces."
    (let* ((file (buffer-file-name))
           (dir  (ignore-errors (pi-coding-agent--session-directory)))
           (rel  (if (and dir (file-in-directory-p file dir))
                     (file-relative-name file dir)
                   file)))
      (my/pi-send-to-input (concat "@" rel " "))))

  :hook ((pi-coding-agent-input-mode . my/pi--setup-image-yank)
         (pi-coding-agent-chat-mode  . agent-chat-mode)
         (pi-coding-agent-input-mode . agent-chat-mode))

  :custom
  (pi-coding-agent-input-markdown-highlighting t)

  :init
  ;; Allow M-x pi as a shorthand
  (defalias 'pi #'pi-coding-agent))

(provide 'setup-ai)
;;; setup-ai.el ends here
