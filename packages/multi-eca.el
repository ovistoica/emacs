;;; multi-eca.el --- Global ECA session dashboard -*- lexical-binding: t; -*-

;; Author: Ovidiu Stoica
;; Keywords: tools, convenience
;; Version: 0.1.0

;;; Commentary:

;; A global "AI IBuffer" dashboard for ECA sessions and chats.
;;
;; `M-x multi-eca' opens the `*multi-eca*' buffer listing every active
;; ECA session and every chat buffer across all projects, grouped by
;; session, with live per-chat status (idle / running / needs
;; attention), cursor-follow preview in a side window, fold support,
;; and management commands (accept tool calls, new/close/rename chat,
;; permanent server-side delete, stop/restart session).
;;
;; The dashboard renders from normalized entry plists collected by the
;; functions in `multi-eca-backends', keeping a thin seam for future
;; non-ECA backends.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'markdown-mode)

(require 'eca-util)
(require 'eca-chat)
(require 'eca)

;; Present in eca-chat.el but possibly missing from an older compiled
;; eca-chat.elc picked up at compile time.
(declare-function eca-chat--switch-to-buffer "eca-chat" (buffer session))
(declare-function eca-chat-delete "eca-chat" ())
(declare-function eca-chat--switch-windows-to-sibling "eca-chat" (session buffer))
(declare-function eca-chat-send-prompt "eca-chat" (prompt))
(declare-function eca-api-request-sync "eca-api")
(declare-function eca-chat-media--extension-for-type "eca-chat-context" (type))
(declare-function eca-chat-completion-at-point "eca-chat-context")
(declare-function eca-chat--completion-type-at-point "eca-chat-context")

(defvar yank-media--registered-handlers)

;;;; Customization

(defgroup multi-eca nil
  "Global dashboard for ECA sessions and chats."
  :prefix "multi-eca-"
  :group 'tools)

(defcustom multi-eca-buffer-name "*multi-eca*"
  "Name of the multi-eca dashboard buffer."
  :type 'string
  :group 'multi-eca)

(defcustom multi-eca-list-display-action nil
  "Display action used by `pop-to-buffer' for the dashboard buffer.
When nil, the default `pop-to-buffer' behavior is used."
  :type '(choice (const :tag "Default" nil) sexp)
  :group 'multi-eca)

(defun multi-eca--balance-window-width (window)
  "Resize side WINDOW to an equal share of the frame columns.
Gives WINDOW the width each window would get after
`balance-windows', instead of a fixed fraction, and then balances
the remaining main windows.  Used as the `window-width' action
alist entry of `multi-eca-display-action'; `balance-windows'
itself never resizes side windows."
  (let* ((frame (window-frame window))
         (total (window-total-width (frame-root-window frame)))
         (mains (seq-remove
                 (lambda (w)
                   (or (eq w window)
                       (window-parameter w 'window-side)))
                 (window-list frame 'no-minibuffer)))
         (share (/ total (1+ (length mains))))
         (delta (- share (window-total-width window))))
    (unless (zerop delta)
      (ignore-errors (window-resize window delta t t)))
    (balance-windows (window-main-window frame))))

(defcustom multi-eca-display-action
  '((display-buffer-in-side-window)
    (side . right)
    (slot . 0)
    (window-width . multi-eca--balance-window-width))
  "Display action used to preview chat buffers from the dashboard.
The default gives the preview window a balanced, equal share of
the frame via `multi-eca--balance-window-width'."
  :type 'sexp
  :group 'multi-eca)

(defcustom multi-eca-preview-delay 0.15
  "Idle seconds before the chat under point is previewed."
  :type 'number
  :group 'multi-eca)

(defcustom multi-eca-fullscreen t
  "Whether opening the dashboard takes over the whole frame.
When non-nil, `multi-eca' saves the current window configuration
and deletes other windows, magit-fullscreen style; quitting with
\\<multi-eca-list-mode-map>\\[multi-eca-quit] restores the saved
configuration."
  :type 'boolean
  :group 'multi-eca)

;;;; Faces

(defface multi-eca-group-heading
  '((t :inherit bold))
  "Face for session group headings in the dashboard."
  :group 'multi-eca)

(defface multi-eca-detail
  '((t :inherit shadow))
  "Face for dim detail text (folders, model, usage, summary)."
  :group 'multi-eca)

(defface multi-eca-chat-idle
  '((t :inherit eca-tab-inactive-face))
  "Face for idle chat lines in the dashboard."
  :group 'multi-eca)

(defface multi-eca-chat-running
  '((t :inherit eca-chat-tab-active-face))
  "Face for running (loading) chat lines in the dashboard."
  :group 'multi-eca)

(defface multi-eca-chat-attention
  '((((background dark)) :foreground "#ff9e64")
    (((background light)) :foreground "#cc5500"))
  "Face for chat lines waiting on the user (pending approval or question)."
  :group 'multi-eca)

(defface multi-eca-chevron
  '((t :inherit success :weight bold))
  "Face for the chevron marking the currently visited chat."
  :group 'multi-eca)

;;;; State

(defvar multi-eca-backends (list #'multi-eca--eca-entries)
  "List of entry-collector functions rendered in the dashboard.
Each function takes no arguments and returns a list of entry
plists: group entries (:type group :name STR :detail STR
:status SYM :session SESSION) followed by their chat entries
\(:type chat :title STR :status SYM :detail STR :buffer BUF
:session SESSION).")

(defvar multi-eca--collapsed (make-hash-table :test #'equal)
  "Hash table of collapsed group fold state, keyed by session id.")

(defvar multi-eca--refresh-timer nil
  "Pending idle timer for a debounced dashboard refresh, or nil.")

(defvar multi-eca--preview-timer nil
  "Pending idle timer for a debounced chat preview, or nil.")

(defvar multi-eca--last-previewed nil
  "The last chat buffer displayed by the cursor-follow preview.")

(defvar multi-eca--previous-window-configuration nil
  "Window configuration saved before the dashboard went fullscreen.
A list of (WINDOW-CONFIGURATION POINT-MARKER), the same shape a
window-configuration register holds, restored by `multi-eca-quit'
via `register-val-jump-to'.")

(defvar-local multi-eca--follow t
  "Non-nil when moving point in the dashboard previews the chat at point.")

;;;; Entry collection

(defun multi-eca--chat-status (buf)
  "Return the status symbol `attention', `running' or `idle' for chat BUF."
  (cond
   ((and (fboundp 'eca-chat--needs-attention-p)
         (eca-chat--needs-attention-p buf))
    'attention)
   ((eq (buffer-local-value 'eca-chat--chat-loading buf) t) 'running)
   (t 'idle)))

(defun multi-eca--chat-detail (buf)
  "Return the model and usage detail string for chat BUF."
  (with-current-buffer buf
    (let* ((model eca-chat--selected-model)
           (usage (and eca-chat--session-cost
                       (fboundp 'eca-chat--usage-str)
                       (ignore-errors
                         (when-let* ((str (eca-chat--usage-str)))
                           (substring-no-properties str))))))
      (string-join (delq nil (list model usage)) "  "))))

(defun multi-eca--chat-entry (session buf)
  "Return a chat entry plist for chat BUF belonging to SESSION."
  (list :type 'chat
        :title (with-current-buffer buf
                 (substring-no-properties (eca-chat-title)))
        :status (multi-eca--chat-status buf)
        :detail (multi-eca--chat-detail buf)
        :buffer buf
        :session session))

(defun multi-eca--group-entry (session)
  "Return a group entry plist for SESSION."
  (let ((folders (mapconcat #'abbreviate-file-name
                            (eca--session-workspace-folders session)
                            ", ")))
    (list :type 'group
          :name (eca--session-project-name session)
          :detail (format "(%s)  [%s]" folders (eca--session-status session))
          :status (eca--session-status session)
          :session session)))

(defun multi-eca--session-live-chats (session)
  "Return the live chat buffers of SESSION."
  (seq-filter #'buffer-live-p (eca-vals (eca--session-chats session))))

(defun multi-eca--eca-entries ()
  "Collect group and chat entries from all ECA sessions."
  (seq-mapcat
   (lambda (session)
     (cons (multi-eca--group-entry session)
           (mapcar (lambda (buf) (multi-eca--chat-entry session buf))
                   (multi-eca--session-live-chats session))))
   (eca-vals eca--sessions)))

(defun multi-eca--collect-entries ()
  "Return dashboard entries from all functions in `multi-eca-backends'."
  (seq-mapcat #'funcall multi-eca-backends))

;;;; Entry helpers

(defun multi-eca--entry-at-point ()
  "Return the entry plist attached to the current line, or nil."
  (get-text-property (line-beginning-position) 'multi-eca-entry))

(defun multi-eca--entry-key (entry)
  "Return a stable relocation key for ENTRY, or nil.
Chat entries use their buffer; group entries use their session id."
  (pcase (plist-get entry :type)
    ('chat (plist-get entry :buffer))
    ('group (when-let* ((session (plist-get entry :session)))
              (eca--session-id session)))
    (_ nil)))

(defun multi-eca--chat-entry-at-point ()
  "Return the chat entry at point, signaling `user-error' when absent."
  (let ((entry (multi-eca--entry-at-point)))
    (unless (and entry
                 (eq (plist-get entry :type) 'chat)
                 (buffer-live-p (plist-get entry :buffer)))
      (user-error "No chat at point"))
    entry))

;;;; Rendering

(defun multi-eca--attention-symbol ()
  "Return the indicator symbol for chats needing attention."
  (if (boundp 'eca-chat-mcp-tool-call-pending-approval-symbol)
      eca-chat-mcp-tool-call-pending-approval-symbol
    "🚧"))

(defun multi-eca--insert-line (text entry)
  "Insert TEXT as a dashboard line carrying ENTRY as a text property.
When ENTRY is nil the line carries no entry property."
  (insert (if entry (propertize text 'multi-eca-entry entry) text) "\n"))

(defun multi-eca--summary-string (entries)
  "Return the dim summary line text for ENTRIES."
  (let* ((chats (seq-filter (lambda (e) (eq (plist-get e :type) 'chat)) entries))
         (sessions (seq-count (lambda (e) (eq (plist-get e :type) 'group)) entries))
         (attention (seq-count (lambda (e) (eq (plist-get e :status) 'attention))
                               chats)))
    (propertize (format "%d sessions · %d chats · %d need attention"
                        sessions (length chats) attention)
                'face 'multi-eca-detail)))

(defun multi-eca--partition-groups (entries)
  "Partition flat ENTRIES into a list of (GROUP . CHATS) conses."
  (let (result current chats)
    (dolist (entry entries)
      (pcase (plist-get entry :type)
        ('group
         (when current
           (push (cons current (nreverse chats)) result))
         (setq current entry
               chats nil))
        ('chat (push entry chats))))
    (when current
      (push (cons current (nreverse chats)) result))
    (nreverse result)))

(defun multi-eca--render-chat (entry)
  "Insert the dashboard line for chat ENTRY."
  (let* ((status (plist-get entry :status))
         (prefix (pcase status
                   ('attention (concat (multi-eca--attention-symbol) " "))
                   ('running "⏳ ")
                   (_ "")))
         (face (pcase status
                 ('attention 'multi-eca-chat-attention)
                 ('running 'multi-eca-chat-running)
                 (_ 'multi-eca-chat-idle)))
         (detail (plist-get entry :detail))
         (visited (when-let* ((buf (plist-get entry :buffer)))
                    (and (buffer-live-p buf) (get-buffer-window buf)))))
    (multi-eca--insert-line
     (concat (if visited
                 (propertize "> " 'face 'multi-eca-chevron)
               "  ")
             (propertize (concat prefix (plist-get entry :title)) 'face face)
             (if (or (null detail) (string-empty-p detail))
                 ""
               (concat "  " (propertize detail 'face 'multi-eca-detail))))
     entry)))

(defun multi-eca--render-group (group chats)
  "Insert the header for GROUP followed by its CHATS entries.
Collapsed groups render only the header line."
  (let* ((session (plist-get group :session))
         (id (and session (eca--session-id session)))
         (collapsed (and id (gethash id multi-eca--collapsed))))
    (multi-eca--insert-line
     (concat (propertize (format "%s %s"
                                 (if collapsed "▶" "▼")
                                 (plist-get group :name))
                         'face 'multi-eca-group-heading)
             " "
             (propertize (or (plist-get group :detail) "")
                         'face 'multi-eca-detail))
     group)
    (unless collapsed
      (if chats
          (dolist (chat chats)
            (multi-eca--render-chat chat))
        (multi-eca--insert-line
         (propertize "  (no chats yet)" 'face 'multi-eca-detail)
         group)))
    (insert "\n")))

(defun multi-eca--render (entries)
  "Render ENTRIES into the current (empty) dashboard buffer."
  (multi-eca--insert-line (multi-eca--summary-string entries) nil)
  (insert "\n")
  (dolist (group (multi-eca--partition-groups entries))
    (multi-eca--render-group (car group) (cdr group))))

;;;; Refresh

(defun multi-eca--restore-point (key line)
  "Move point to the line whose entry matches KEY, else to LINE."
  (goto-char (point-min))
  (let ((found nil))
    (when key
      (while (and (not found) (not (eobp)))
        (if (equal key (multi-eca--entry-key (multi-eca--entry-at-point)))
            (setq found t)
          (forward-line 1))))
    (unless found
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun multi-eca--refresh (&rest _)
  "Re-collect entries and re-render the dashboard, preserving point."
  (setq multi-eca--refresh-timer nil)
  (when-let* ((buf (get-buffer multi-eca-buffer-name)))
    (with-current-buffer buf
      (let ((key (multi-eca--entry-key (multi-eca--entry-at-point)))
            (line (line-number-at-pos))
            (inhibit-read-only t))
        (erase-buffer)
        (multi-eca--render (multi-eca--collect-entries))
        (multi-eca--restore-point key line)
        (dolist (window (get-buffer-window-list buf nil t))
          (set-window-point window (point)))))))

(defun multi-eca--schedule-refresh (&rest _)
  "Schedule a debounced dashboard refresh.
No-op when the dashboard buffer does not exist."
  (when (or (get-buffer-window multi-eca-buffer-name t)
            (get-buffer multi-eca-buffer-name))
    (when (timerp multi-eca--refresh-timer)
      (cancel-timer multi-eca--refresh-timer))
    (setq multi-eca--refresh-timer
          (run-with-idle-timer 0.1 nil #'multi-eca--refresh))))

(defun multi-eca--on-status-change (_session)
  "Schedule a dashboard refresh after a session status transition."
  (multi-eca--schedule-refresh))

(defun multi-eca--on-chat-kill ()
  "Schedule a dashboard refresh when an ECA chat buffer is killed."
  (when (derived-mode-p 'eca-chat-mode)
    (multi-eca--schedule-refresh)))

(defun multi-eca--on-chat-opened (&rest _)
  "Schedule a dashboard refresh when a chat is opened or forked.
Installed as an after advice on `eca-chat-opened' so new chats
\(forks, resumes, replays) appear in the dashboard immediately."
  (multi-eca--schedule-refresh))

(defun multi-eca--on-window-change (_frame)
  "Schedule a refresh when window contents change while visible.
Keeps the currently-visited chevron accurate when chat windows are
shown or hidden through any code path (RET, preview, `C-x 1',
window deletions)."
  (when (get-buffer-window multi-eca-buffer-name t)
    (multi-eca--schedule-refresh)))

(defun multi-eca--revert (&rest _)
  "Buffer-local `revert-buffer-function' for the dashboard."
  (multi-eca--refresh))

;;;; Context sync & preview

(defun multi-eca--sync-line-context (entry)
  "Pin `eca-session' resolution to ENTRY's session for this buffer.
Sets the buffer-local session-id cache and `default-directory', and
when ENTRY carries a chat buffer marks it as the session's last chat
buffer, so `eca-transient-menu' and eca commands act on the line at
point."
  (when-let* ((session (plist-get entry :session)))
    (setq-local eca--session-id-cache (eca--session-id session))
    (when-let* ((dir (car (eca--session-workspace-folders session))))
      (setq-local default-directory dir))
    (when-let* ((buf (plist-get entry :buffer)))
      (when (buffer-live-p buf)
        (setf (eca--session-last-chat-buffer session) buf)))))

(defun multi-eca--preview (entry)
  "Display ENTRY's chat buffer in the preview window without selecting it.
Skips work when the buffer is dead or already the last previewed one.
Moves the preview window point to the end only when the buffer was not
already visible."
  (setq multi-eca--preview-timer nil)
  (let ((buf (plist-get entry :buffer))
        (session (plist-get entry :session)))
    (when (and (buffer-live-p buf)
               (not (eq buf multi-eca--last-previewed)))
      (setq multi-eca--last-previewed buf)
      (when session
        (setf (eca--session-last-chat-buffer session) buf))
      (let* ((was-visible (get-buffer-window buf t))
             (window (display-buffer buf multi-eca-display-action)))
        (when (and window (not was-visible))
          (set-window-point window (with-current-buffer buf (point-max))))))))

(defun multi-eca--schedule-preview (entry)
  "Schedule a debounced preview of chat ENTRY."
  (when (timerp multi-eca--preview-timer)
    (cancel-timer multi-eca--preview-timer))
  (setq multi-eca--preview-timer
        (run-with-idle-timer multi-eca-preview-delay nil
                             #'multi-eca--preview entry)))

(defun multi-eca--post-command ()
  "Sync per-line context and schedule the chat preview after commands."
  (when-let* ((entry (multi-eca--entry-at-point)))
    (multi-eca--sync-line-context entry)
    (when (and multi-eca--follow
               (eq (plist-get entry :type) 'chat))
      (multi-eca--schedule-preview entry))))

;;;; Navigation

(defun multi-eca--find-line (step pred)
  "Return position of the nearest line in STEP direction matching PRED.
STEP is 1 to search forward or -1 to search backward.  PRED is called
with the entry of each candidate line; return nil when no line matches."
  (save-excursion
    (let (result)
      (while (and (not result)
                  (zerop (forward-line step)))
        (let ((entry (multi-eca--entry-at-point)))
          (when (and entry (funcall pred entry))
            (setq result (point)))))
      result)))

(defun multi-eca--chat-line-p (entry)
  "Return non-nil when ENTRY is a chat entry."
  (eq (plist-get entry :type) 'chat))

(defun multi-eca--attention-line-p (entry)
  "Return non-nil when ENTRY is a chat entry needing attention."
  (and (multi-eca--chat-line-p entry)
       (eq (plist-get entry :status) 'attention)))

(defun multi-eca-next-chat ()
  "Move point to the next chat line."
  (interactive)
  (if-let* ((pos (multi-eca--find-line 1 #'multi-eca--chat-line-p)))
      (goto-char pos)
    (message "No more chats")))

(defun multi-eca-previous-chat ()
  "Move point to the previous chat line."
  (interactive)
  (if-let* ((pos (multi-eca--find-line -1 #'multi-eca--chat-line-p)))
      (goto-char pos)
    (message "No more chats")))

(defun multi-eca-next-attention ()
  "Move point to the next chat line needing attention."
  (interactive)
  (if-let* ((pos (multi-eca--find-line 1 #'multi-eca--attention-line-p)))
      (goto-char pos)
    (message "No chats need attention")))

(defun multi-eca-previous-attention ()
  "Move point to the previous chat line needing attention."
  (interactive)
  (if-let* ((pos (multi-eca--find-line -1 #'multi-eca--attention-line-p)))
      (goto-char pos)
    (message "No chats need attention")))

;;;; Commands

(defun multi-eca-refresh ()
  "Refresh the dashboard immediately."
  (interactive)
  (multi-eca--refresh))

(defun multi-eca-quit ()
  "Quit the dashboard, restoring the window layout saved on entry.
Buries the dashboard buffer and jumps back to the window
configuration and point captured by `multi-eca' before it went
fullscreen, like `magit-quit'.  Falls back to `quit-window' when
no configuration was saved."
  (interactive)
  (let ((prev multi-eca--previous-window-configuration))
    (setq multi-eca--previous-window-configuration nil)
    (if prev
        (progn
          (bury-buffer (current-buffer))
          (register-val-jump-to prev nil))
      (quit-window))))

(defun multi-eca--goto-group (id)
  "Move point to the group header line for session ID.
Return non-nil when the header was found."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((entry (multi-eca--entry-at-point)))
        (if (and (eq (plist-get entry :type) 'group)
                 (equal id (multi-eca--entry-key entry)))
            (setq found t)
          (forward-line 1))))
    found))

(defun multi-eca-toggle-fold ()
  "Toggle folding of the session group at point.
Point ends up on the group's header line, also when the fold was
toggled from one of the group's chat lines."
  (interactive)
  (let* ((entry (multi-eca--entry-at-point))
         (session (and entry (plist-get entry :session)))
         (id (and session (eca--session-id session))))
    (unless id
      (user-error "No session at point"))
    (if (gethash id multi-eca--collapsed)
        (remhash id multi-eca--collapsed)
      (puthash id t multi-eca--collapsed))
    (multi-eca--refresh)
    (multi-eca--goto-group id)))

(defun multi-eca-tab ()
  "Toggle folding on a project line, otherwise jump to the next chat.
Mirrors TAB in Info buffers: on a project (group) header the
project's chats are folded or unfolded; on any other line point
moves to the next chat entry."
  (interactive)
  (let ((entry (multi-eca--entry-at-point)))
    (if (eq (plist-get entry :type) 'group)
        (multi-eca-toggle-fold)
      (multi-eca-next-chat))))

(defun multi-eca-toggle-follow ()
  "Toggle the cursor-follow chat preview in the dashboard."
  (interactive)
  (setq multi-eca--follow (not multi-eca--follow))
  (message "multi-eca follow %s" (if multi-eca--follow "enabled" "disabled")))

(defun multi-eca-preview ()
  "Toggle the preview of the chat at point without selecting its window.
When the chat is not visible, display it in the preview window.
When it is already visible, hide its window instead."
  (interactive)
  (let* ((entry (multi-eca--chat-entry-at-point))
         (buf (plist-get entry :buffer))
         (win (get-buffer-window buf)))
    (if win
        (progn
          ;; Keep `multi-eca--last-previewed' pointing at BUF so the
          ;; cursor-follow preview does not immediately re-show the
          ;; window while point stays on this line.
          (setq multi-eca--last-previewed buf)
          (delete-window win))
      (setq multi-eca--last-previewed nil)
      (multi-eca--preview entry))))

(defun multi-eca-visit ()
  "Focus the chat at point, or toggle folding when on a group header."
  (interactive)
  (let ((entry (multi-eca--entry-at-point)))
    (pcase (and entry (plist-get entry :type))
      ('chat (eca-chat--switch-to-buffer (plist-get entry :buffer)
                                         (plist-get entry :session)))
      ('group (multi-eca-toggle-fold))
      (_ (user-error "Nothing at point")))))

(defun multi-eca-accept-tool-calls ()
  "Accept every pending tool call of the chat at point."
  (interactive)
  (let ((entry (multi-eca--chat-entry-at-point)))
    (multi-eca--sync-line-context entry)
    (if (fboundp 'eca-chat-tool-call-accept-all)
        (eca-chat-tool-call-accept-all)
      (user-error "Function `eca-chat-tool-call-accept-all' is not available"))))

(defun multi-eca-new-chat ()
  "Start a new chat in the session at point."
  (interactive)
  (let ((entry (multi-eca--entry-at-point)))
    (unless (and entry (plist-get entry :session))
      (user-error "No session at point"))
    (multi-eca--sync-line-context entry)
    (eca-chat-new)))

(defun multi-eca-close-chat ()
  "Close the chat buffer at point without deleting it server-side.
The chat history is preserved and can be reopened later via
`eca-chat-resume'.  Any window showing the chat switches to a
sibling chat first.  Use \\[multi-eca-delete-chat] to delete a
chat permanently."
  (interactive)
  (let* ((entry (multi-eca--chat-entry-at-point))
         (buf (plist-get entry :buffer))
         (session (plist-get entry :session))
         (chat-id (buffer-local-value 'eca-chat--id buf)))
    (when session
      (eca-chat--switch-windows-to-sibling session buf)
      (when chat-id
        (setf (eca--session-chats session)
              (eca-dissoc (eca--session-chats session) chat-id))))
    ;; Mark the chat closed so eca's `kill-buffer' hook neither prompts
    ;; about server-side deletion nor re-runs the cleanup done above.
    (with-current-buffer buf
      (setq-local eca-chat--closed t))
    (kill-buffer buf)
    (multi-eca--schedule-refresh)))

(defun multi-eca-delete-chat ()
  "Permanently delete the chat at point from the server, then refresh.
This removes the chat from the server history — it will no longer
be available via `eca-chat-resume'.  Use \\[multi-eca-close-chat]
to merely close the buffer while keeping the history."
  (interactive)
  (let* ((entry (multi-eca--chat-entry-at-point))
         (buf (plist-get entry :buffer)))
    (when (y-or-n-p (format "Permanently delete chat %S from server history? "
                            (plist-get entry :title)))
      (with-current-buffer buf
        (eca-chat-delete))
      (multi-eca--schedule-refresh))))

(defun multi-eca-rename-chat ()
  "Rename the chat at point, then refresh."
  (interactive)
  (let ((entry (multi-eca--chat-entry-at-point)))
    (multi-eca--sync-line-context entry)
    (call-interactively #'eca-chat-rename)
    (multi-eca--schedule-refresh)))

(defun multi-eca--chat-fork-candidates (buf)
  "Return fork-point candidates from the user messages of chat BUF.
Each element is (LABEL . CONTENT-ID), newest message first.  LABEL
is the numbered first line of the message, truncated for display."
  (with-current-buffer buf
    (let ((ovs (seq-sort-by
                #'overlay-start #'<
                (seq-filter
                 (lambda (ov) (overlay-get ov 'eca-chat--user-message-id))
                 (overlays-in (point-min) (point-max))))))
      (nreverse
       (seq-map-indexed
        (lambda (ov idx)
          (let* ((start (overlay-start ov))
                 (text (save-excursion
                         (goto-char start)
                         (buffer-substring-no-properties start (line-end-position)))))
            (cons (format "%d │ %s" (1+ idx)
                          (truncate-string-to-width (string-trim text) 70 nil nil "…"))
                  (overlay-get ov 'eca-chat--user-message-id))))
        ovs)))))

(defun multi-eca--fork-completion-table (labels)
  "Return a completion table over LABELS preserving their order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (category . multi-eca-fork)
                   (display-sort-function . identity))
      (complete-with-action action labels string pred))))

(defun multi-eca-fork-chat ()
  "Fork the chat at point into a new chat.
Prompts for the fork point: \"latest\" copies the full chat history
and settings (the server's native /fork command), while picking a
user message forks the history up to and including that message,
dropping everything after it."
  (interactive)
  (let* ((entry (multi-eca--chat-entry-at-point))
         (buf (plist-get entry :buffer))
         (session (plist-get entry :session))
         (candidates (cons (cons "latest (full copy)" 'latest)
                           (multi-eca--chat-fork-candidates buf)))
         (choice (completing-read
                  "Fork from: "
                  (multi-eca--fork-completion-table (mapcar #'car candidates))
                  nil t))
         (target (cdr (assoc choice candidates))))
    (multi-eca--sync-line-context entry)
    (if (eq target 'latest)
        (eca-chat-send-prompt "/fork")
      (eca-api-request-sync
       session
       :method "chat/fork"
       :params (list :chatId (buffer-local-value 'eca-chat--id buf)
                     :contentId target)))
    (multi-eca--schedule-refresh)))

(defun multi-eca-select-model ()
  "Select the model used by the chat at point."
  (interactive)
  (let ((entry (multi-eca--chat-entry-at-point)))
    (multi-eca--sync-line-context entry)
    (call-interactively #'eca-chat-select-model)
    (multi-eca--schedule-refresh)))

(defun multi-eca-select-agent ()
  "Select the agent (behavior/prompt) used by the chat at point."
  (interactive)
  (let ((entry (multi-eca--chat-entry-at-point)))
    (multi-eca--sync-line-context entry)
    (call-interactively #'eca-chat-select-agent)
    (multi-eca--schedule-refresh)))

(defun multi-eca-compact-chat ()
  "Compact the chat at point via the server's /compact command.
Summarizes the chat so far, cleaning previous history to reduce
context.  Prompts for optional additional input guiding the
summary; leave it empty to compact with the default behavior."
  (interactive)
  (let* ((entry (multi-eca--chat-entry-at-point))
         (input (string-trim
                 (read-string "Compact additional input (optional): "))))
    (multi-eca--sync-line-context entry)
    (eca-chat-send-prompt
     (if (string-empty-p input)
         "/compact"
       (concat "/compact " input)))
    (multi-eca--schedule-refresh)))

(defun multi-eca--session-name-at-point (entry)
  "Return the project name of ENTRY's session, erroring when absent."
  (let ((session (and entry (plist-get entry :session))))
    (unless session
      (user-error "No session at point"))
    (eca--session-project-name session)))

(defun multi-eca-stop-session ()
  "Stop the ECA session at point after confirmation, then refresh.
Stopping shuts the server down and kills all its chat buffers."
  (interactive)
  (let* ((entry (multi-eca--entry-at-point))
         (name (multi-eca--session-name-at-point entry)))
    (when (y-or-n-p (format "Stop ECA session %S and kill its chats? " name))
      (multi-eca--sync-line-context entry)
      (eca-stop)
      (multi-eca--schedule-refresh))))

(defun multi-eca-restart-session ()
  "Restart the ECA session at point after confirmation, then refresh.
Restarting stops the server (killing its chat buffers) and starts
a fresh session for the same workspace."
  (interactive)
  (let* ((entry (multi-eca--entry-at-point))
         (name (multi-eca--session-name-at-point entry)))
    (when (y-or-n-p (format "Restart ECA session %S (kills its chats)? " name))
      (multi-eca--sync-line-context entry)
      (eca-restart)
      (multi-eca--schedule-refresh))))

;;;; Compose

(defvar-local multi-eca-compose--target-buffer nil
  "The chat buffer the composed prompt will be sent to.")

(defvar-local multi-eca-compose--session nil
  "The ECA session owning `multi-eca-compose--target-buffer'.")

(defun multi-eca-compose--yank-image-handler (type data)
  "Save clipboard image DATA of MIME TYPE and insert an @file mention.
Writes the image to a temporary eca-screenshot file, like the eca
chat buffer does, and inserts \"@/path/to/file \" at point so the
server picks it up as a file context when the prompt is sent."
  (let* ((extension (eca-chat-media--extension-for-type type))
         (output-path (make-temp-file "eca-screenshot-" nil
                                      (concat "." extension))))
    (condition-case err
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region data nil output-path nil 'silent))
          (insert "@" output-path " ")
          (message "Image added: %s (%s)" output-path
                   (file-size-human-readable
                    (file-attribute-size (file-attributes output-path)))))
      (error
       (user-error "Failed to save yanked image: %s"
                   (error-message-string err))))))

(defun multi-eca-compose--clipboard-image-p ()
  "Return non-nil when an image is available on the clipboard."
  (when-let* ((targets (and (display-graphic-p)
                            (gui-get-selection 'CLIPBOARD 'TARGETS))))
    (seq-some (lambda (type)
                (and (symbolp type)
                     (string-match-p "\\`image/" (symbol-name type))))
              (if (vectorp targets) (append targets nil) targets))))

(defun multi-eca-compose--suppress-chat-fields (orig-fn &rest args)
  "Call ORIG-FN with ARGS unless inside a compose buffer.
Around advice for eca's chat-field predicates
\(`eca-chat--point-at-new-context-p' and
`eca-chat--point-at-prompt-field-p'): they false-positive in
buffers without the chat overlays (the missing context-area start
point makes `line-number-at-pos' fall back to the current line),
which would route completion to chat-only code paths.  In compose
buffers they must simply report nil."
  (unless (derived-mode-p 'multi-eca-compose-mode)
    (apply orig-fn args)))

(defun multi-eca-compose-completion-at-point ()
  "Complete @context and #filepath mentions via the ECA server.
Reuses `eca-chat-completion-at-point' (chat/queryContext and
chat/queryFiles) against the compose buffer's target chat."
  (eca-chat-completion-at-point))

(defun multi-eca-compose-tab ()
  "Complete the @/# mention at point, else do markdown cycling.
With point after an @context or #filepath prefix this triggers
`completion-at-point'; anywhere else it behaves like TAB in
`markdown-mode'."
  (interactive)
  (if (eca-chat--completion-type-at-point)
      (completion-at-point)
    (call-interactively #'markdown-cycle)))

(defun multi-eca-compose-yank ()
  "Yank into the compose buffer, routing images through `yank-media'.
A clipboard image is saved to a temporary file and inserted as an
@file mention; anything else falls back to a plain `yank'."
  (interactive)
  (if (and (fboundp 'yank-media)
           yank-media--registered-handlers
           (multi-eca-compose--clipboard-image-p))
      (call-interactively #'yank-media)
    (call-interactively #'yank)))

(define-derived-mode multi-eca-compose-mode markdown-mode "ECA-Compose"
  "Major mode for composing a prompt destined for an ECA chat.
The target chat is captured when the buffer is created by
`multi-eca-compose'.  Yanking a clipboard image inserts an @file
mention pointing at a temporary screenshot file, mirroring the
eca chat buffer behavior.

\\{multi-eca-compose-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "Compose prompt: \\[multi-eca-compose-send] to send, \
\\[multi-eca-compose-cancel] to cancel"))
  ;; ECA server completion for @contexts and #filepaths, mirroring the
  ;; chat prompt setup including its completion-style overrides.
  (setq-local completion-at-point-functions
              (list #'multi-eca-compose-completion-at-point))
  (setq-local completion-category-overrides
              (cons '(eca-capf (styles basic substring))
                    completion-category-overrides))
  ;; Paste image from clipboard support, mirroring eca-chat-mode:
  ;; drop the handlers inherited from markdown-mode (which insert
  ;; markdown image links) and register the @file mention handler.
  (when (fboundp 'yank-media-handler)
    (setq-local yank-media--registered-handlers nil)
    (dolist (mime '("image/png" "image/jpeg" "image/jpg"
                    "image/gif" "image/webp"))
      (yank-media-handler mime #'multi-eca-compose--yank-image-handler))))

(define-key multi-eca-compose-mode-map (kbd "C-c C-c") #'multi-eca-compose-send)
(define-key multi-eca-compose-mode-map (kbd "C-c C-k") #'multi-eca-compose-cancel)
(define-key multi-eca-compose-mode-map [remap yank] #'multi-eca-compose-yank)
(define-key multi-eca-compose-mode-map (kbd "TAB") #'multi-eca-compose-tab)
(define-key multi-eca-compose-mode-map (kbd "<tab>") #'multi-eca-compose-tab)

(defun multi-eca-compose ()
  "Compose a prompt for the chat at point in a markdown buffer.
Opens a `multi-eca-compose-mode' buffer targeting the chat under
the cursor.  \\<multi-eca-compose-mode-map>\\[multi-eca-compose-send] \
sends the buffer content as a prompt to that chat;
\\[multi-eca-compose-cancel] discards it."
  (interactive)
  (let* ((entry (multi-eca--chat-entry-at-point))
         (target (plist-get entry :buffer))
         (session (plist-get entry :session))
         (title (plist-get entry :title))
         (buf (generate-new-buffer (format "*eca-compose: %s*" title))))
    (with-current-buffer buf
      (multi-eca-compose-mode)
      (setq multi-eca-compose--target-buffer target
            multi-eca-compose--session session)
      ;; Make `eca-session' resolve to the target's session from this
      ;; buffer, so the send path works regardless of where the compose
      ;; window ends up.
      (setq-local eca--session-id-cache (eca--session-id session))
      (when-let* ((dir (car (eca--session-workspace-folders session))))
        (setq-local default-directory dir))
      ;; Carry the target's chat id so @/# completion queries
      ;; (chat/queryContext, chat/queryFiles) run against that chat.
      (setq-local eca-chat--id
                  (buffer-local-value 'eca-chat--id target)))
    (pop-to-buffer buf)
    (message "Composing for %s" title)))

(defun multi-eca-compose-send ()
  "Send the composed prompt to the captured target chat.
Kills the compose buffer afterwards."
  (interactive)
  (let ((text (string-trim
               (buffer-substring-no-properties (point-min) (point-max))))
        (target multi-eca-compose--target-buffer)
        (session multi-eca-compose--session))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (unless (buffer-live-p target)
      (user-error "The target chat buffer no longer exists"))
    (setf (eca--session-last-chat-buffer session) target)
    (eca-chat-send-prompt text)
    (quit-window t)
    (message "Prompt sent to %s" (buffer-name target))))

(defun multi-eca-compose-cancel ()
  "Discard the composed prompt and kill the compose buffer."
  (interactive)
  (quit-window t)
  (message "Compose cancelled"))

;;;; Major mode

(defvar multi-eca-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'multi-eca-visit)
    (define-key map (kbd "TAB") #'multi-eca-tab)
    (define-key map (kbd "<tab>") #'multi-eca-tab)
    (define-key map (kbd "<backtab>") #'multi-eca-previous-chat)
    (define-key map (kbd "SPC") #'multi-eca-preview)
    (define-key map (kbd "g") #'multi-eca-refresh)
    (define-key map (kbd "n") #'multi-eca-next-chat)
    (define-key map (kbd "p") #'multi-eca-previous-chat)
    (define-key map (kbd "M-n") #'multi-eca-next-attention)
    (define-key map (kbd "M-p") #'multi-eca-previous-attention)
    (define-key map (kbd "f") #'multi-eca-toggle-follow)
    (define-key map (kbd "a") #'multi-eca-accept-tool-calls)
    (define-key map (kbd "+") #'multi-eca-new-chat)
    (define-key map (kbd "k") #'multi-eca-close-chat)
    (define-key map (kbd "D") #'multi-eca-delete-chat)
    (define-key map (kbd "M") #'multi-eca-select-model)
    (define-key map (kbd "V") #'multi-eca-select-agent)
    (define-key map (kbd "c") #'multi-eca-compose)
    (define-key map (kbd "C") #'multi-eca-compact-chat)
    (define-key map (kbd "F") #'multi-eca-fork-chat)
    (define-key map (kbd "r") #'multi-eca-rename-chat)
    (define-key map (kbd "R") #'multi-eca-rename-chat)
    (define-key map (kbd "S") #'multi-eca-stop-session)
    (define-key map (kbd "X") #'multi-eca-restart-session)
    (define-key map (kbd "q") #'multi-eca-quit)
    map)
  "Keymap for `multi-eca-list-mode'.")

(define-derived-mode multi-eca-list-mode special-mode "Multi-ECA"
  "Major mode for the global ECA sessions and chats dashboard.

\\{multi-eca-list-mode-map}"
  (setq-local truncate-lines t
              revert-buffer-function #'multi-eca--revert)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'multi-eca--post-command nil t))

;;;###autoload
(defun multi-eca ()
  "Open the global ECA dashboard listing all sessions and chats.
When `multi-eca-fullscreen' is non-nil, the current window
configuration is saved first and the dashboard takes over the
frame; \\<multi-eca-list-mode-map>\\[multi-eca-quit] restores it."
  (interactive)
  (let ((buf (get-buffer-create multi-eca-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'multi-eca-list-mode)
        (multi-eca-list-mode))
      (multi-eca--refresh))
    (when (and multi-eca-fullscreen
               (not (get-buffer-window buf)))
      (setq multi-eca--previous-window-configuration
            (list (current-window-configuration) (point-marker))))
    (pop-to-buffer buf multi-eca-list-display-action)
    (when multi-eca-fullscreen
      (delete-other-windows))))

;;;; Global hooks (added once at load; refresh no-ops when buffer is absent)

(add-hook 'eca-chat-session-status-changed-functions #'multi-eca--on-status-change)
(add-hook 'eca-after-initialize-hook #'multi-eca--schedule-refresh)
(add-hook 'kill-buffer-hook #'multi-eca--on-chat-kill)
(add-hook 'window-buffer-change-functions #'multi-eca--on-window-change)
(advice-add 'eca-chat-opened :after #'multi-eca--on-chat-opened)
(advice-add 'eca-chat--point-at-new-context-p :around
            #'multi-eca-compose--suppress-chat-fields)
(advice-add 'eca-chat--point-at-prompt-field-p :around
            #'multi-eca-compose--suppress-chat-fields)

(provide 'multi-eca)
;;; multi-eca.el ends here
