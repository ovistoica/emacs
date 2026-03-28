;;; setup-pi-coding-agent.el --- Pi coding agent Emacs frontend -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for pi-coding-agent, an Emacs frontend for the pi CLI
;; coding agent (https://github.com/dnouri/pi-coding-agent).
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

;;; Code:

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

  (defun my/pi--mode-to-lang ()
    "Return a code-fence language string for the current buffer's major mode."
    (pcase major-mode
      ((or 'clojure-mode 'clojurec-mode)  "clojure")
      ('clojurescript-mode                "clojurescript")
      ((or 'emacs-lisp-mode
           'lisp-interation-mode)        "emacs-lisp")
      ((or 'python-mode 'python-ts-mode)  "python")
      ((or 'js-mode 'js-ts-mode)          "javascript")
      ((or 'typescript-mode
           'typescript-ts-mode)           "typescript")
      ('tsx-ts-mode                       "tsx")
      ((or 'rust-mode 'rust-ts-mode)      "rust")
      ((or 'go-mode 'go-ts-mode)          "go")
      ((or 'sh-mode 'bash-ts-mode)        "bash")
      ((or 'css-mode 'css-ts-mode)        "css")
      ((or 'html-mode 'mhtml-mode)        "html")
      ((or 'json-mode 'json-ts-mode)      "json")
      ((or 'yaml-mode 'yaml-ts-mode)      "yaml")
      ('sql-mode                          "sql")
      (_
       (string-remove-suffix
        "-ts-mode"
        (string-remove-suffix "-mode" (symbol-name major-mode))))))

  (defun my/pi-region-to-string (beg end)
    "Return a fenced code block string for the region from BEG to END.
The block is prefixed with the file path (relative to project root) and
line range, e.g.:
  `src/foo.clj` L42-67:
  ```clojure
  (defn my-fn ...)
  ```"
    (let* ((file     (buffer-file-name))
           (root     (when-let* ((proj (project-current)))
                       (project-root proj)))
           (rel-file (if (and file root)
                         (file-relative-name file root)
                       (buffer-name)))
           (start-line (line-number-at-pos beg))
           (end-line   (let ((l (line-number-at-pos end)))
                         (if (and (> end beg)
                                  (save-excursion (goto-char end) (bolp)))
                             (1- l) l)))
           (lang (my/pi--mode-to-lang))
           (code (buffer-substring-no-properties beg end))
           (code (if (string-suffix-p "\n" code) code (concat code "\n"))))
      (format "`%s` L%d-%d:\n```%s\n%s```\n"
              rel-file start-line end-line lang code)))

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
        (my/pi-send-to-input (my/pi-region-to-string beg end))
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

(provide 'setup-pi-coding-agent)
;;; setup-pi-coding-agent.el ends here
