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
  `(defadvice ,command (around ,(intern (concat "wrap-" (symbol-name command) "-fullscreen")) activate)
     (let ((my/prev (list (current-window-configuration) (point-marker))))
       ad-do-it
       (delete-other-windows)
       (setq-local my/previous-window-configuration my/prev))))

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

;; No need to remind me about eldoc-mode all the time
(diminish 'eldoc-mode)

(provide 'tooling)
