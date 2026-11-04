;;; setup-babashka.el --- Babashka task runner integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides integration with Babashka task runner (bb.edn).
;; Supports running tasks from bb.edn, Makefile, and package.json.

;;; Code:

(require 'dash)
(require 's)

(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")

(declare-function makefile-invoke-target "setup-makefile-mode")
(declare-function js-pkg-run "setup-js")

(defun babashka--parse-tasks-output (output)
  "Return the task names listed in `bb tasks' OUTPUT.
Names are the first word of each line after the \"The following tasks\"
header.  Return nil when there is no header, e.g. \"No tasks found.\"."
  (when-let* ((lines (split-string output "\n"))
              (body (cdr (--drop-while (not (string-prefix-p "The following tasks" it))
                                       lines))))
    (->> body
         (--map (car (split-string it nil t)))
         (-non-nil))))

(defun babashka--error-summary (stderr)
  "Return a readable summary of `bb' STDERR.
Dependency download progress and Java stack frames are dropped;
\"(no error output)\" is returned when nothing is left."
  (let* ((noise "\\`\\(?:Cloning:\\|Checking out:\\|Downloading:\\|at \\)")
         (summary (->> (split-string stderr "\n" t "[ \t\r]+")
                       (--remove (string-match-p noise it))
                       (s-join "\n"))))
    (if (string-empty-p summary) "(no error output)" summary)))

(defun babashka-find-tasks (&optional root)
  "Return the babashka task names of the project at ROOT.
ROOT defaults to `default-directory'.  Runs `bb tasks' and signals a
`user-error' summarizing stderr when it exits with a non-zero status."
  (let ((default-directory (or root default-directory))
        (stderr-file (make-temp-file "bb-tasks-stderr")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (process-file "bb" nil (list t stderr-file) nil "tasks")))
            (if (eql status 0)
                (babashka--parse-tasks-output (buffer-string))
              (user-error "Running bb tasks failed (exit %s): %s" status
                          (babashka--error-summary
                           (with-temp-buffer
                             (insert-file-contents stderr-file)
                             (buffer-string)))))))
      (delete-file stderr-file))))

(defun shorten-path (path)
  "Shortens the file PATH by replacing the home directory with ~."
  (let ((home (expand-file-name "~")))
    (if (string-prefix-p home path)
        (concat "~" (substring path (length home)))
      path)))

(defun babashka--exec-task (cmd)
  "Execute babashka CMD using compile with comint for interactive input."
  (let* ((project-name (projectile-project-name))
         (compilation-buffer-name-function
          (lambda (_mode)
            (format "*bb:%s - %s*" project-name cmd))))
    (compile cmd t)))

(defun babashka-invoke-task ()
  "Invoke a babashka task from bb.edn."
  (interactive)
  (let* ((root (projectile-project-root))
         (short-dir (shorten-path root))
         (default-directory root))
    (if (not (file-exists-p (expand-file-name "bb.edn" root)))
        (message "No bb.edn found in %s" short-dir)
      (let ((tasks (babashka-find-tasks root)))
        (unless tasks
          (user-error "No babashka tasks found in %s" short-dir))
        (babashka--exec-task
         (completing-read (format "bb in %s: " short-dir)
                          (--map (concat "bb " it) tasks)))))))

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
