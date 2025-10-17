(defun babashka-find-tasks ()
  "Extract the task names of all the tasks from the bb.edn buffer"
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward ":tasks" nil t)
      ;; Move past the :tasks key and any whitespace/opening brace
      (skip-chars-forward " \t\n{")

      ;; Skip past :requires if it exists
      (when (looking-at ":requires")
        (forward-sexp 2))         ; Skip :requires and its associated vector/sexp

      (let ((tasks '()))
        (while (< (point) (point-max))
          (skip-chars-forward " \t\n,") ;; Skip tabs, spaces, newlines, commas
          (when (< (point) (point-max))
            (cond
             ;; Found a task name
             ((looking-at "\\([a-zA-Z0-9_:-]+\\)")

              (let ((task-name (match-string-no-properties 0)))
                (unless (string-prefix-p "requires" task-name)
                  (push task-name tasks)))
              (goto-char (match-end 0))
              ;; Skip the associated value (map, vector, or atom)
              (skip-chars-forward " \t\n")
              (forward-sexp))

             ;; Skip comments and other content
             ((looking-at ";")
              (forward-line))

             ;; Move forward if we hit something unexpected
             (t (forward-char)))))
        (reverse tasks)))))

(defun shorten-path (path)
  "Shortens the file PATH by replacing the home directory with ~."
  (let ((home (expand-file-name "~")))
    (if (string-prefix-p home path)
        (concat "~" (substring path (length home)))
      path)))

(defvar babashka--previous-window-configuration nil)
(defvar babashka--previous-task nil)

(defun babashka-run-task (task task-buffer-name)
  (async-shell-command task task-buffer-name)
  (unless (s-equals? (buffer-name) task-buffer-name)
    (switch-to-buffer-other-window task-buffer-name))
  (setq-local babashka--previous-window-configuration prev)
  (setq-local babashka--previous-task task)
  (read-only-mode)
  (local-set-key (kbd "m") 'babashka-invoke-task)
  (local-set-key (kbd "g") (λ (babashka-invoke-task t)))
  (local-set-key (kbd "q") (λ (let ((conf babashka--previous-window-configuration))
                                (kill-buffer)
                                (when conf (register-val-jump-to conf nil))))))

(defun babashka-invoke-task (&optional repeat?)
  (interactive)
  (let* ((file (concat (projectile-project-root) "bb.edn"))
         (short-dir (shorten-path (projectile-project-root)))
         (default-directory (projectile-project-root))
         (task-buffer-name (concat "*BB Task " (shorten-path (projectile-project-root)) "*"))
         (prev (if (get-buffer task-buffer-name)
                   (with-current-buffer task-buffer-name
                     babashka--previous-window-configuration)
                 (list (current-window-configuration) (point-marker))))
         (task (or (and repeat? (with-current-buffer task-buffer-name
                                  babashka--previous-task))
                   (completing-read (format "bb in %s" short-dir)
                                    (--map
                                     (concat "bb " it)
                                     (with-temp-buffer
                                       (insert-file-contents file)
                                       (babashka-find-tasks)))))))
    (if (file-exists-p file)
        (babashka-run-task task task-buffer-name)
        (message "No bb.edn found in %s" short-dir))))

(defun my/task-runner ()
  "Run task runner based on the project in the list of priorities:
1. bb.edn - babashka-invoke-task
2. Makefile - makefile-invoke-target
3. package.json - js-pkg-run"
  (interactive)
  (let* ((root (projectile-project-root))
         (bb-edn (concat root "bb.edn"))
         (makefile (concat root "Makefile"))
         (pkg-json (concat root "package.json")))
    (cond
     ((file-exists-p bb-edn)
      (babashka-invoke-task))
     ((file-exists-p makefile)
      (makefile-invoke-target))
     ((file-exists-p pkg-json)
      (call-interactively #'js-pkg-run))
     (t (message "No task file found. Add %s %s or %s" (shorten-path bb-edn) (shorten-path makefile) (shorten-path pkg-json))))))

(global-set-key (kbd "s-m") 'my/task-runner)

(provide 'setup-babashka)
;; setup-babashka.el ends here
