;;; setup-ai.el --- AI tooling configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-related packages.
;;
;; Sections:
;;   1. agent-shell  (xenodium/agent-shell, ACP-based)
;;   2. eca          (editor-code-assistant/eca-emacs)
;;   3. claude-code-ide (manzaltu/claude-code-ide.el — parsnips' ghostel fork)
;;   4. pi-coding-agent (dnouri/pi-coding-agent)

;;; Code:

(require 'transient)
(require 'buffers)

;; ─────────────────────────────────────────────────────────────────────────────
;; 1. agent-shell
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
  :bind (("C-c C-;" . agent-shell)
         ("C-c ;"   . my/agent-shell-toggle)
         ("C-c '"   . agent-shell-menu))
  :custom
  ;; Window configuration
  (agent-shell-display-action
   '((display-buffer-in-side-window)
     (side . right)
     (window-width . 90)))
  ;; Show icons for different agent configs
  (agent-shell-show-config-icons t)
  ;; Expand tool use blocks by default
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
     [("f" "Send file" agent-shell-send-file)
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
;; 2. eca — Editor Code Assistant (vendor-neutral agent interface)
;; ─────────────────────────────────────────────────────────────────────────────

(defun my/eca-fix-autoloads ()
  "Regenerate ECA autoloads if broken by package-vc.
package-vc sometimes generates a stub autoloads file that loads itself
recursively instead of containing actual autoload definitions.  This
detects and fixes that by regenerating autoloads properly."
  (when-let* ((pkg-dir (and (package-installed-p 'eca)
                            (package-desc-dir (cadr (assq 'eca package-alist)))))
              (autoloads-file (expand-file-name "eca-autoloads.el" pkg-dir)))
    (when (and (file-exists-p autoloads-file)
               (with-temp-buffer
                 (insert-file-contents autoloads-file)
                 ;; Broken if it only has the self-referencing stub
                 (and (search-forward "Autoload indirection" nil t)
                      (not (search-forward ";;; Generated autoloads" nil t)))))
      (message "ECA: Regenerating broken autoloads...")
      (package-generate-autoloads "eca" pkg-dir)
      ;; Remove the self-referencing stub and add proper header
      (with-temp-buffer
        (insert-file-contents autoloads-file)
        (goto-char (point-min))
        (when (search-forward "Autoload indirection" nil t)
          (let ((stub-end (search-forward "\n\n" nil t)))
            (when stub-end
              (delete-region (point-min) stub-end)
              (goto-char (point-min))
              ;; Proper header with load-path setup (critical for package-vc)
              (insert ";;; eca-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-\n"
                      ";;\n;;; Code:\n\n"
                      "(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))\n\n")
              ;; Add provide/footer if missing
              (goto-char (point-max))
              (unless (search-backward "(provide 'eca-autoloads)" nil t)
                (goto-char (point-max))
                (insert "\n(provide 'eca-autoloads)\n;;; eca-autoloads.el ends here\n"))
              (write-region (point-min) (point-max) autoloads-file))))
        (load autoloads-file nil t)
        (message "ECA: Autoloads regenerated successfully.")))))

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

(use-package eca
  :defines (eca-chat-focus-on-open)
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

  (defun my/eca-ensure-chat-window-visible (&rest _)
    "Ensure the ECA chat window is visible before interacting with it."
    (when-let* ((session (eca-session))
                (buffer (eca-chat--get-last-buffer session)))
      (unless (get-buffer-window buffer t)
        (let ((eca-chat-focus-on-open nil))
          (eca-chat--display-buffer buffer)))))

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
  :hook (eca-chat-mode . my/eca-chat-mode-hook)
  :bind (("C-c ." . eca-transient-menu)
         ("C-c e" . eca-chat-toggle-window)
         ("C-c i" . eca-chat-add-context-to-user-prompt)
         (:map eca-chat-mode-map
               ("C-c C-o" . my/eca-chat-expand-all-blocks)
               ("C-c C-c" . my/eca-chat-collapse-all-blocks)))
  :ensure t
  :config
  ;; Ensure chat window is visible before adding context
  (advice-add 'eca-chat-add-context-to-user-prompt :before #'my/eca-ensure-chat-window-visible)

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

  :custom
  (eca-chat-auto-add-repomap t)
  (eca-worktree-mode 'isolated)
  ;; Keep tool call blocks open after they complete — no manual tabbing needed.
  (eca-chat-shrink-called-tools nil))

;; ─────────────────────────────────────────────────────────────────────────────
;; 3. claude-code-ide
;;    Using parsnips' fork with ghostel backend support (PR #190 upstream).
;;    Once merged upstream, switch back to:
;;      :vc (:url "https://github.com/manzaltu/claude-code-ide.el")
;; ─────────────────────────────────────────────────────────────────────────────

(use-package claude-code-ide
  :vc (:url "https://github.com/parsnips/claude-code-ide.el" :branch "codex/ghostel-backend-support")
  :bind ("C-c C-'" . claude-code-ide-menu)
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
  (claude-code-ide-emacs-tools-setup))

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
;; KEYBINDINGS
;;
;;   C-c C-p   Toggle the pi side panel for the current project.
;;             Opens chat (top) + input (bottom) on the right without
;;             disturbing your existing window layout.  Press again to
;;             close.  If no session exists for the project yet, one is
;;             created automatically.
;;
;;   C-c TAB   `my/pi-send-dwim' — context-aware send:
;;             • With an active region: formats the selected code as a
;;               fenced block annotated with the relative file path and
;;               line range (e.g. `src/foo.clj` L42-67:), then appends
;;               it to the pi input buffer and focuses it so you can
;;               type your question immediately.
;;             • Without a region: prompts for free-form text in the
;;               minibuffer and sends that instead.
;;
;;   C-c C-m   Open the pi menu (inside pi buffers).
;;             Frees up C-c C-p in pi's own keymaps for the toggle above.
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
;;   `my/pi-region-to-string' BEG END
;;       Pure function.  Returns a markdown fenced code block string for
;;       the region, with a `file` L<start>-<end> header.  Language is
;;       inferred from the buffer's major mode.  Easy to compose with
;;       other send targets beyond pi.
;;
;;   `my/pi-send-to-input' TEXT
;;       Appends TEXT to the pi input buffer for the current project,
;;       opens the side panel if hidden, and focuses the input window.
;;       Use this to build your own send commands (e.g. send a compile
;;       error, a git diff hunk, a CIDER exception, etc.).
;;
;; CIDER / CLOJURE NOTE
;;   C-c C-p was previously bound to `cider-pprint-eval-last-sexp' in
;;   cider-mode-map and to `nrepl-warn-when-not-connected' in
;;   clojure-mode-map.  Both are unbound in setup-cider.el to free the
;;   key for the pi toggle.
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
With an active region, formats it with file path and line range and sends it.
Without a region, reads a message from the minibuffer and sends that."
    (interactive "r")
    (if (use-region-p)
        (my/pi-send-to-input (region-to-string beg end))
      (let ((text (read-string "Send to pi: ")))
        (unless (string-empty-p text)
          (my/pi-send-to-input (concat text "\n"))))))

  :hook
  (pi-coding-agent-input-mode . my/pi--setup-image-yank)

  :custom
  (pi-coding-agent-input-markdown-highlighting t)

  :init
  ;; Allow M-x pi as a shorthand
  (defalias 'pi 'pi-coding-agent)

  :bind (("C-c C-p" . my/pi-coding-agent-toggle)
         ("C-c TAB" . my/pi-send-dwim)
         :map pi-coding-agent-input-mode-map
         ("C-c C-m" . pi-coding-agent-menu)
         ("C-c C-p" . my/pi-coding-agent-toggle)
         :map pi-coding-agent-chat-mode-map
         ("C-c C-m" . pi-coding-agent-menu)
         ("C-c C-p" . my/pi-coding-agent-toggle)))

(provide 'setup-ai)
;;; setup-ai.el ends here
