;; setup-pi-coding-agent.el --- Pi coding agent Emacs frontend

(use-package pi-coding-agent
  :vc (:url "https://github.com/dnouri/pi-coding-agent" :rev :newest)
  :bind (("C-c C-p"   . my/pi-coding-agent-toggle)
         ("C-c TAB"   . my/pi-send-dwim)
         :map pi-coding-agent-input-mode-map
         ("C-c C-m" . pi-coding-agent-menu)
         ("C-c C-p" . my/pi-coding-agent-toggle)
         :map pi-coding-agent-chat-mode-map
         ("C-c C-m" . pi-coding-agent-menu)
         ("C-c C-p" . my/pi-coding-agent-toggle))
  :init
  ;; Allow M-x pi as a shorthand
  (defalias 'pi 'pi-coding-agent)
  :config

  (defvar my/pi-coding-agent--saved-window-config nil
    "Window configuration saved before showing the pi coding agent layout.")

  (defun my/pi-coding-agent-toggle ()
    "Toggle a focused pi layout: chat on top, input on bottom.
On first press, saves the current window configuration and shows the
pi session for the current project fullscreen (split into two panes).
On second press, restores the previous window configuration.
Signals an error if no pi session exists for the current project."
    (interactive)
    (let* ((dir (pi-coding-agent--session-directory))
           (chat-buf (pi-coding-agent--find-session dir))
           (input-buf (and chat-buf
                           (buffer-local-value
                            'pi-coding-agent--input-buffer chat-buf))))
      (cond
       ;; Pi windows are visible → restore previous layout or bury buffers
       ((and chat-buf (get-buffer-window chat-buf))
        (if my/pi-coding-agent--saved-window-config
            (set-window-configuration my/pi-coding-agent--saved-window-config)
          (with-current-buffer chat-buf
            (pi-coding-agent--hide-session-windows)))
        (setq my/pi-coding-agent--saved-window-config nil))

       ;; No session yet → start one, then show it
       ;; Session exists but hidden → save layout and show it
       (t
        (setq my/pi-coding-agent--saved-window-config
              (current-window-configuration))
        (unless chat-buf
          (setq chat-buf (pi-coding-agent--setup-session dir))
          (setq input-buf (buffer-local-value
                           'pi-coding-agent--input-buffer chat-buf)))
        (delete-other-windows)
        (switch-to-buffer chat-buf)
        (let* ((input-height (or (bound-and-true-p pi-coding-agent-input-window-height) 10))
               (input-win (split-window-below (- (window-total-height) input-height))))
          (set-window-buffer input-win input-buf)
          (select-window input-win))))))

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
    (let* ((file  (buffer-file-name))
           (root  (when-let* ((proj (project-current)))
                    (project-root proj)))
           (rel-file   (if (and file root)
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
Creates a new pi session if one does not exist yet.  If the pi windows are
not currently visible, saves the window configuration and opens the layout."
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
        (setq my/pi-coding-agent--saved-window-config (current-window-configuration))
        (delete-other-windows)
        (switch-to-buffer chat-buf)
        (let* ((input-height (or (bound-and-true-p pi-coding-agent-input-window-height) 10))
               (input-win (split-window-below (- (window-total-height) input-height))))
          (set-window-buffer input-win input-buf)
          (select-window input-win)))
      (select-window (get-buffer-window input-buf))
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
          (my/pi-send-to-input (concat text "\n")))))))
