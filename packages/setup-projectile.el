;;; setup-projectile.el --- Projectile project interaction setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Projectile is a project interaction library.  It provides a nice set of
;; features operating on a project level without introducing external
;; dependencies.  This module wires Projectile up with perspectives so that
;; switching projects also switches workspaces.

;;; Code:

(declare-function projectile-switch-project-by-name "projectile")
(declare-function projectile-project-root "projectile")
(declare-function projectile-mode "projectile")
(declare-function projectile-find-file "projectile")
(declare-function projectile-consult-find-file "projectile-consult")
(declare-function with-perspective "setup-perspective")
(declare-function s-contains? "s")

(defvar projectile-command-map)
(defvar projectile-ignored-project-function)
(defvar projectile-switch-project-action)

(defun my/projectile-switch-project-to-emacs ()
  "Switch to the Emacs configuration project.
Prefer \"~/.config/emacs\" when it exists, otherwise fall back to
the classic \"~/.emacs.d\" location."
  (interactive)
  (let ((emacs-dir (if (file-directory-p "~/.config/emacs")
                       "~/.config/emacs/"
                     "~/.emacs.d/")))
    (projectile-switch-project-by-name emacs-dir)))

(defun my/projectile-run-tmux ()
  "Open Ghostty in the project root with a tmux session named after the project.
Attaches to an existing session if one already exists."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (session-name (current-project-name))
         (cmd (format "tmux new-session -A -s '%s'" session-name)))
    (start-process "ghostty" nil "ghostty"
                   (format "--working-directory=%s" project-root)
                   "-e" "sh" "-c" cmd)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-switch-project-by-name)

  :bind-keymap
  (("s-p" . projectile-command-map))

  :bind
  ("C-x p p" . projectile-switch-project)
  ("C-x p e" . my/projectile-switch-project-to-emacs)

  :config
  (projectile-mode +1)
  (define-key projectile-command-map (kbd "s-p") #'projectile-switch-project)
  (define-key projectile-command-map (kbd "x m") #'my/projectile-run-tmux)

  (setq projectile-ignored-project-function 'my/ignore-project?)

  (require 'setup-perspective)
  (require 'project-processes)

  ;; Post-switch action:
  ;;   - first visit to a project -> new perspective + `projectile-find-file'
  ;;   - returning to a project    -> restore its perspective, landing you in
  ;;                                  the most recently visited file
  (setq projectile-switch-project-action #'switch-perspective+find-file))

(use-package projectile-consult
  ;; Ships inside the projectile package (3.0+); consult-powered
  ;; streaming/previewing variants of the projectile commands.
  :ensure nil
  :after (projectile consult)
  :bind
  (([remap projectile-find-file] . projectile-consult-find-file)))

(use-package project-processes
  :ensure nil
  :defer t
  :after (projectile prodigy))

(defun current-project-name ()
  "Return the name of the current Projectile project.
This is the final directory component of the project root."
  (cadr (reverse (split-string (projectile-project-root) "/"))))

(defun switch-perspective+find-file ()
  "Switch to the current project's perspective, finding a file on first visit.
On the first visit a perspective is created and `projectile-find-file'
prompts for a file.  On subsequent visits the existing perspective is
restored, landing point in the most recently visited file."
  (with-perspective (current-project-name)
    (if (fboundp 'projectile-consult-find-file)
        (projectile-consult-find-file)
      (projectile-find-file))))

(defun my/ignore-project? (file-name)
  "Return non-nil when FILE-NAME belongs to a project that should be ignored.
Currently ignores Clojure's \".gitlibs\" dependency checkouts."
  (s-contains? ".gitlibs" file-name))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
