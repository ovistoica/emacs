;;; tooling.el --- General tooling utilities -*- lexical-binding: t; -*-

(use-package s)
(use-package dash)
(use-package diminish)

;; Shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(defun my/insert-lambda-symbol ()
  (interactive)
  (insert "\u03bb"))

(global-set-key (kbd "s-l") 'my/insert-lambda-symbol)

;; Set up a keybinding for the very next command invocation
(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map)
   t))

;; Instrument a `command' to store the current window configuration in
;; `register' and then going fullscreen.
(defmacro wrap-fullscreen (command)
  `(advice-add ',command :around
               (lambda (orig-fn &rest args)
                 (let ((my/prev (list (current-window-configuration) (point-marker))))
                   (apply orig-fn args)
                   (delete-other-windows)
                   (setq-local my/previous-window-configuration my/prev)))
               '((name . ,(concat "wrap-" (symbol-name command) "-fullscreen")))))

(defvar my/previous-window-configuration nil)

(defun my/copy-buffer-as-prompt ()
  "Copy the current buffer content with file information for AI prompts."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (project-root (when file-path (project-root (project-current))))
         (relative-path (if (and file-path project-root)
                            (file-relative-name file-path project-root)
                          (or (buffer-name) "untitled")))
         (file-extension (when file-path (file-name-extension file-path)))
         (buffer-content (buffer-string))
         (prompt-text (format "@%s\n%s"
                              relative-path
                              buffer-content)))
    (kill-new prompt-text)
    (message "Buffer copied as prompt: %s" relative-path)))

(global-set-key (kbd "C-s-w") 'my/copy-buffer-as-prompt)

(defvar async-cmd--previous-window-configuration nil)

(defun quittable-async-shell-command (command &optional quit-immediately-p)
  "Like async-shell-command but with small improvements on the result
buffer. Execute `COMMAND' in separate buffer, asynchronously. Kill the
result buffer immediately if `QUIT-IMMEDIATELY-P' is t"
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Async shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Async shell command: ")
                        nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))
    nil))
  (let* ((cmd-buffer-name (concat "*Command: " command "*"))
         (current-buffer (buffer-name))
         (prev (if (get-buffer cmd-buffer-name)
                   (with-current-buffer cmd-buffer-name
                     async-cmd--previous-window-configuration)
                 (list (current-window-configuration) (point-marker)))))
    (progn
      (async-shell-command command cmd-buffer-name)
      (unless (s-equals? (buffer-name) cmd-buffer-name)
        (switch-to-buffer-other-window cmd-buffer-name))
      (setq async-cmd--previous-window-configuration prev)
      (read-only-mode)
      (local-set-key (kbd "g") (λ (async-shell-command command cmd-buffer-name)))
      (local-set-key (kbd "q") (λ (let ((conf async-cmd--previous-window-configuration))
                                    (kill-buffer)
                                    (when conf (register-val-jump-to conf nil)))))
      (when quit-immediately-p
        (let ((conf async-cmd--previous-window-configuration))
          (kill-buffer)
          (when conf (register-val-jump-to conf nil)))))))

(global-set-key (kbd "M-&") 'quittable-async-shell-command)


;; Load bash history into shell-command-history
(defun my/load-bash-history ()
  "Load bash history into `shell-command-history'."
  (let ((bash-history-file "~/.bash_history"))
    (when (file-exists-p bash-history-file)
      (with-temp-buffer
        (insert-file-contents bash-history-file)
        (let ((lines (split-string (buffer-string) "\n" t)))
          ;; Add unique entries, most recent first
          (setq shell-command-history
                (delete-dups (append (nreverse lines) shell-command-history))))))))

(defun my/setup-romanian-diacritics ()
  "Set up Romanian diacritics input."
  (interactive)
  (local-set-key (kbd "s-a") (λ (insert "ă")))
  (local-set-key (kbd "s-q") (λ (insert "â")))
  (local-set-key (kbd "s-t") (λ (insert "ț")))
  (local-set-key (kbd "s-i") (λ (insert "î")))
  (local-set-key (kbd "s-s") (λ (insert "ș")))
  (local-set-key (kbd "s-A") (λ (insert "Ă")))
  (local-set-key (kbd "s-Q") (λ (insert "Â")))
  (local-set-key (kbd "s-T") (λ (insert "Ț")))
  (local-set-key (kbd "s-I") (λ (insert "Î")))
  (local-set-key (kbd "s-S") (λ (insert "Ș"))))

;; Insert pressed keybindings as a (kbd ...) form
(defun my/read-and-insert-key-sequence ()
  "Read key events until ESC, then insert them as a `kbd' form at point."
  (interactive)
  (let ((keys []))
    (while-let ((event (read-event (format "Press keys (ESC when done): %s"
                                           (key-description keys))))
                ((not (memq event '(escape ?\e)))))
      (setq keys (vconcat keys (vector event))))
    (when (< 0 (length keys))
      (insert (format "(kbd \"%s\")" (key-description keys))))))

(with-eval-after-load 'elisp-mode
  (keymap-set emacs-lisp-mode-map "C-M-S-s-k" #'my/read-and-insert-key-sequence))

;; Find keys that are free across modifier combinations
(defconst my/modifier-families
  '("C-" "M-" "s-" "H-" "C-S-" "C-M-" "C-s-" "M-s-"
    "C-M-S-" "C-M-s-" "C-S-s-" "M-S-s-")
  "Modifier prefixes compared by `my/find-free-modifier-families'.")

(defconst my/modifier-family-keys
  (append (number-sequence ?a ?z)
          (number-sequence ?A ?Z)
          (number-sequence ?0 ?9)
          '(?, ?\; ?. ?: ?- ?_ ?@ ?* ?+ ?? ?< ?> ?' ?! ?\" ?# ?$ ?% ?& ?/
               ?\( ?\) ?= ?`))
  "Base keys checked by `my/find-free-modifier-families'.")

(defun my/key-free? (key)
  "Return non-nil if KEY (a `kbd' string) is unbound in the current buffer."
  (memq (key-binding (kbd key)) '(nil undefined)))

(defun my/modifier-family-row (char)
  "Return (KEY . FREE-FLAGS) for CHAR across `my/modifier-families'."
  (let ((key (char-to-string char)))
    (cons key (mapcar (lambda (modifier) (my/key-free? (concat modifier key)))
                      my/modifier-families))))

(defun my/render-modifier-family-rows (rows)
  "Render ROWS from `my/modifier-family-row' as a text table."
  (let* ((cell (lambda (s) (format "%-7s" s)))
         (line (lambda (key cells total)
                 (concat (format "%-5s" key) (mapconcat cell cells "") total "\n")))
         (total (length my/modifier-families)))
    (concat
     (funcall line "Key" my/modifier-families "Free")
     (funcall line "----" (mapcar (lambda (_) "------") my/modifier-families) "----")
     (mapconcat (lambda (row)
                  (funcall line (car row)
                           (mapcar (lambda (free) (if free "·" "✗")) (cdr row))
                           (format "%d/%d" (seq-count #'identity (cdr row)) total)))
                rows ""))))

(defun my/find-free-modifier-families (&optional require-all)
  "Show which keys are unbound across modifier combinations.
Display a table with one column per entry in `my/modifier-families'.
With prefix argument REQUIRE-ALL, only show keys that are free across
all modifier combinations."
  (interactive "P")
  (let* ((rows (seq-filter (lambda (row) (or (not require-all)
                                             (seq-every-p #'identity (cdr row))))
                           (mapcar #'my/modifier-family-row my/modifier-family-keys)))
         (table (my/render-modifier-family-rows rows)))
    (with-current-buffer (get-buffer-create "*Free Modifier Families*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert table)
        (goto-char (point-min)))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;; Load on startup
(my/load-bash-history)

;; No need to remind me about eldoc-mode all the time
(diminish 'eldoc-mode)

(provide 'tooling)
;;; tooling.el ends here
