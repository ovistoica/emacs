;;; setup-babashka.el --- Babashka task runner integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides integration with Babashka task runner (bb.edn).
;; Supports running tasks from bb.edn, Makefile, and package.json.

;;; Code:

(require 'dash)
(require 's)
(require 'projectile)

(declare-function makefile-invoke-target "setup-makefile-mode")
(declare-function js-pkg-run "setup-js")

(defun babashka-find-tasks ()
  "Extract the task names of all the tasks from the bb.edn buffer."
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

(defun babashka--exec-task (cmd)
  "Execute babashka CMD using compile with a task-specific buffer name."
  (let* ((project-name (projectile-project-name))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*bb:%s - %s*" project-name cmd))))
    (compile cmd)))

(defun babashka-invoke-task ()
  "Invoke a babashka task from bb.edn."
  (interactive)
  (let* ((file (concat (projectile-project-root) "bb.edn"))
         (short-dir (shorten-path (projectile-project-root)))
         (default-directory (projectile-project-root))
         (task (completing-read (format "bb in %s: " short-dir)
                                (--map
                                 (concat "bb " it)
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (babashka-find-tasks))))))
    (if (file-exists-p file)
        (babashka--exec-task task)
      (message "No bb.edn found in %s" short-dir))))

(defun display-prefix (arg)
  "Display the value of the raw prefix ARG."
  (interactive "P")
  (message "%s" arg))

(defvar available-task-file-lists
  '(("Babashka (bb.edn)" . babashka-invoke-task)
    ("Makefile (Makefile)" . makefile-invoke-target)
    ("JS Task (package.json)" . (lambda () (call-interactively #'js-pkg-run))))
  "Alist of task runner names to their invoke functions.")

(defvar-local preferred-task-runner nil
  "Buffer local variable to set in case of multiple task runner files detected.
Example: (nil . ((preferred-task-runner . makefile)))")

(defun handle-preferred-task-runner ()
  "Run the task runner specified by `preferred-task-runner'."
  (cond
   ((eq preferred-task-runner 'bb)
    (babashka-invoke-task))
   ((eq preferred-task-runner 'makefile)
    (makefile-invoke-target))
   ((eq preferred-task-runner 'pkg-json)
    (call-interactively #'js-pkg-run))
   (t (message "Invalid preferred task runner. Valid options are 'bb, 'makefile or 'pkg-json. Got %s" preferred-task-runner))))

(defun my/task-runner (arg)
  "Run task runner based on project files.
With prefix ARG, prompt to choose task runner.
Priority order: bb.edn, Makefile, package.json."
  (interactive "P")
  (let* ((root (projectile-project-root))
         (bb-edn (concat root "bb.edn"))
         (makefile (concat root "Makefile"))
         (pkg-json (concat root "package.json")))
    (if arg
        (let* ((task-runner (completing-read "Choose task runner: " (mapcar #'car available-task-file-lists)))
               (task-fn (cdr (assoc task-runner available-task-file-lists))))
          (funcall task-fn))
      (if preferred-task-runner
          (handle-preferred-task-runner)
        (cond
         ((file-exists-p bb-edn)
          (babashka-invoke-task))
         ((file-exists-p makefile)
          (makefile-invoke-target))
         ((file-exists-p pkg-json)
          (call-interactively #'js-pkg-run))
         (t (message "No task file found. Add %s %s or %s" (shorten-path bb-edn) (shorten-path makefile) (shorten-path pkg-json))))))))

(global-set-key (kbd "s-m") 'my/task-runner)

(provide 'setup-babashka)
;;; setup-babashka.el ends here
