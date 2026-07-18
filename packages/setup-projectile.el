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
(defvar consult-async-split-style)
(defvar consult-async-split-styles-alist)

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
  ;; `C-x p' mirrors `s-p' 1:1 — both prefixes share `projectile-command-map'.
  ;; This intentionally replaces the built-in project.el prefix on `C-x p'.
  (("s-p" . projectile-command-map)
   ("C-x p" . projectile-command-map))

  :config
  (projectile-mode +1)
  (define-key projectile-command-map (kbd "s-p") #'projectile-switch-project)
  (define-key projectile-command-map (kbd "x m") #'my/projectile-run-tmux)
  ;; Shadows `projectile-recentf' — jump to the Emacs config project instead.
  (define-key projectile-command-map (kbd "e") #'my/projectile-switch-project-to-emacs)

  (setq projectile-ignored-project-function 'my/ignore-project?)

  (require 'setup-perspective)
  (require 'project-processes)

  ;; Post-switch action:
  ;;   - first visit to a project -> new perspective + `projectile-find-file'
  ;;   - returning to a project    -> restore its perspective, landing you in
  ;;                                  the most recently visited file
  (setq projectile-switch-project-action #'switch-perspective+find-file))

(defun my/consult--split-filter-only (_str &optional _plist)
  "Async split style sending nothing to the process.
The whole minibuffer input is used for client-side narrowing by the
completion style (async string empty, filter starts at position 0)."
  '("" 0))

(defun my/projectile-consult-filter-only (orig-fn &rest args)
  "Call ORIG-FN with a filter-only async split style.
`projectile-consult-find-file' runs Projectile's indexing command once
and ignores the async input, so with the default perl split style plain
typing is swallowed by the process instead of narrowing.  Route the
whole input to the completion style instead."
  (let ((consult-async-split-style 'filter-only))
    (apply orig-fn args)))

(use-package projectile-consult
  ;; Ships inside the projectile package (3.0+); consult-powered
  ;; streaming/previewing variants of the projectile commands.
  :ensure nil
  :after (projectile consult)
  :bind
  (([remap projectile-find-file] . projectile-consult-find-file))
  :config
  (add-to-list 'consult-async-split-styles-alist
               (list 'filter-only :function #'my/consult--split-filter-only))
  (advice-add 'projectile-consult-find-file
              :around #'my/projectile-consult-filter-only))

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
