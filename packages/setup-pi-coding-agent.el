;;; setup-pi-coding-agent.el --- Pi coding agent Emacs frontend -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for pi-coding-agent, an Emacs frontend for the pi CLI coding
;; agent (https://github.com/dnouri/pi-coding-agent).
;;
;; Provides:
;;   - A toggleable right side panel (C-c C-p) with chat on top and input
;;     below, leaving the main window layout untouched.  Uses Emacs side
;;     windows so C-x 1 in the main area never kills the panel.
;;   - `my/pi-send-dwim' (C-c TAB): with an active region, sends it as a
;;     fenced code block annotated with file path and line range; without a
;;     region, prompts for free-form text via the minibuffer.
;;   - `my/pi-region-to-string': pure function that formats a buffer region
;;     into a labelled fenced code block string.
;;   - `my/pi-send-to-input': appends any text to the pi input buffer and
;;     opens the side panel if it is not already visible.

;;; Code:

(use-package pi-coding-agent
  :vc (:url "https://github.com/dnouri/pi-coding-agent" :rev :newest)

  :preface
  ;; Silence byte-compiler warnings for pi-coding-agent internals used below.
  (declare-function pi-coding-agent--session-directory "pi-coding-agent")
  (declare-function pi-coding-agent--find-session       "pi-coding-agent")
  (declare-function pi-coding-agent--setup-session      "pi-coding-agent")

  (defcustom my/pi-panel-width 0.35
    "Width of the pi side panel as a fraction of the frame width."
    :type 'number
    :group 'pi-coding-agent)

  (defun my/pi--open-panel (chat-buf input-buf)
    "Display CHAT-BUF (top) and INPUT-BUF (bottom) as a right side panel.
Uses side windows so the main window layout is left untouched."
    (let ((input-height (or (bound-and-true-p pi-coding-agent-input-window-height) 10))
          (win-params   '((no-delete-other-windows . t))))
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
          (window-parameters . ,win-params)))))

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
        (my/pi--open-panel chat-buf input-buf)
        (when-let ((win (get-buffer-window input-buf)))
          (select-window win)))))

  (defun my/pi--mode-to-lang ()
    "Return a code-fence language string for the current buffer's major mode."
    (pcase major-mode
      ((or 'clojure-mode 'clojurec-mode)  "clojure")
      ('clojurescript-mode                "clojurescript")
      ((or 'emacs-lisp-mode
           'lisp-interaction-mode)        "emacs-lisp")
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
        (my/pi--open-panel chat-buf input-buf))
      (when-let ((win (get-buffer-window input-buf)))
        (select-window win))
      (goto-char (point-max))))

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

;;; setup-pi-coding-agent.el ends here
