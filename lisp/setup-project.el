;;; setup-project.el --- Project setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package project
  :ensure t
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-vc-extra-root-markers
   '("Cargo.toml" "compile_commands.json" "pyproject.toml"
     "compile_flags.txt" "project.clj"
     "deps.edn" "shadow-cljs.edn" "package.json"))
  :preface
  (defcustom project-compilation-mode nil
    "Mode to run the `compile' command with."
    :type 'symbol
    :group 'project
    :safe #'symbolp
    :local t)
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))
  (defvar project-compilation-modes nil
    "List of functions to check for specific compilation mode.

The function must return a symbol of an applicable compilation
mode.")
  (define-advice compilation-start
      (:filter-args (args) use-project-compilation-mode)
    (let ((cmd (car args))
          (mode (cadr args))
          (rest (cddr args)))
      (catch 'args
        (when (null mode)
          (dolist (comp-mode-p project-compilation-modes)
            (when-let ((mode (funcall comp-mode-p)))
              (throw 'args (append (list cmd mode) rest)))))
        args)))
  (define-advice project-root (:filter-return (project) abbreviate-project-root)
    (abbreviate-file-name project))
  (defun project-make-predicate-buffer-in-project-p ()
    (let ((project-buffers (project-buffers (project-current))))
      (lambda () (memq (current-buffer) project-buffers))))
  (define-advice project-compile (:around (fn) save-project-buffers-only)
    "Only ask to save project-related buffers."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (project-make-predicate-buffer-in-project-p)))
      (funcall fn)))
  (define-advice recompile
      (:around (fn &optional edit-command) save-project-buffers-only)
    "Only ask to save project-related buffers if inside of a project."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (if (project-current)
               (project-make-predicate-buffer-in-project-p)
             compilation-save-buffers-predicate)))
      (funcall fn edit-command)))
  :config
  (add-to-list 'project-switch-commands
               '(project-dired "Dired"))
  (add-to-list 'project-switch-commands
               '(project-switch-to-buffer "Switch buffer"))
  (add-to-list 'project-switch-commands
               '(project-compile "Compile"))
  (add-to-list 'project-switch-commands
               '(project-save-some-buffers "Save") t))

(use-package projectile
  :ensure projectile
  :diminish
  :defines
  projectile-mode-map
  :functions
  projectile-cleanup-known-projects
  projectile-find-file
  projectile-mode
  projectile-parent
  projectile-project-p
  projectile-project-root
  :hook
  (projectile-after-switch-project . os/node-project-setup)
  :bind
  ("C-c a"   . projectile-ag)
  ("C-c C-o" . projectile-multi-occur)
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1)
  :custom
  (projectile-cache-file
   (expand-file-name ".projectile.cache" os/emacs-tmp-dir))
  (projectile-globally-ignored-files '("TAGS" ".git" ".DS_Store"))
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" os/emacs-tmp-dir))
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-cleanup-known-projects))

(provide 'setup-project)
;;; setup-project.el ends here
