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

(defun my/insert-lambda-symbol ()
  (interactive)
  (insert "\u03bb"))

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

;; No need to remind me about eldoc-mode all the time
(diminish 'eldoc-mode)

(provide 'tooling)


