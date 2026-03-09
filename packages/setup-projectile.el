;; Projectile
;;
;; A project interaction library. It provides a nice set of features operating
;; on a project level without introducing external dependencies

(defun my/projectile-switch-project-to-emacs ()
  (interactive)
  (let ((emacs-dir (if (file-directory-p "~/.config/emacs")
                       "~/.config/emacs/"
                     "~/.emacs.d/")))
    (projectile-switch-project-by-name emacs-dir)))

(use-package projectile
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

  (setq projectile-ignored-project-function 'my/ignore-project?)

  (require 'setup-perspective)
  (require 'project-processes)
  (setq projectile-switch-project-action 'my/projectile-switch-project-action))

(use-package project-processes
  :ensure nil
  :defer t
  :after (projectile prodigy)
  )

(defun current-project-name ()
  (cadr (reverse (split-string (projectile-project-root) "/"))))

(defun switch-perspective+find-file ()
  (with-perspective (current-project-name)
    (projectile-find-file)))

;; Project switch action: switch perspective first, then offer commander menu
(defun my/projectile-switch-project-action ()
  "Switch to the project's perspective, then offer a commander menu.
On first visit the perspective is initialized and the menu is shown.
On subsequent visits the perspective is simply restored and the menu shown."
  (with-perspective (current-project-name)
    (projectile-commander)))

;; Commander methods — keys chosen to avoid conflicts with projectile defaults
(def-projectile-commander-method ?f
                                 "Find file in project."
                                 (projectile-find-file))

(def-projectile-commander-method ?d
                                 "Open project root in Dired."
                                 (projectile-dired))

(def-projectile-commander-method ?v
                                 "Open vterm in project root."
                                 (projectile-run-vterm))

(def-projectile-commander-method ?e
                                 "Open eshell in project root."
                                 (projectile-run-eshell))

(def-projectile-commander-method ?c
                                 "Start an ECA session in project root."
                                 (let ((default-directory (projectile-project-root)))
                                   (if (fboundp 'eca)
                                       (eca)
                                     (message "ECA is not available"))))

(defun my/ignore-project? (file-name)
  (s-contains? ".gitlibs" file-name))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
