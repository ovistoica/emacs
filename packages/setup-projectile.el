;; Projectile
;;
;; A project interaction library. It provides a nice set of features operating
;; on a project level without introducing external dependencies

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
  :bind ((:map projectile-mode-map
               ;; Individual service control
               ("C-x p M-s" . my/projectile-start-prodigy-process)
               ("C-x p M-S" . my/projectile-stop-prodigy-process)

               ;; Bulk operations
               ("C-x p M-a" . my/projectile-start-all-prodigy-services)
               ("C-x p M-A" . my/projectile-stop-all-prodigy-services)
               ("C-x p M-r" . my/projectile-restart-all-prodigy-services)
               ))
  :config
  (add-hook 'projectile-before-switch-project-hook #'project-processes-track-project-state)
  (add-hook 'projectile-after-switch-project-hook #'project-processes-project-switch-hook))

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

(defun my/projectile-switch-project-to-emacs ()
  (interactive)
  (let ((emacs-dir (if (file-directory-p "~/.config/emacs")
                       "~/.config/emacs/"
                     "~/.emacs.d/")))
    (projectile-switch-project-by-name emacs-dir)))


;; Interactive commands for manual control
(defun my/projectile-start-prodigy-process ()
  "Start a prodigy process for the current project.
Shows completion for all stopped services tagged with the current project name."
  (interactive)
  (let* ((project-tag (project-processes-project-tag))
         (stopped-services (project-processes-stopped-services-with-tag project-tag)))
    (if (not project-tag)
        (message "Not in a projectile project")
      (if (not stopped-services)
          (message "No stopped services found for project: %s" project-tag)
        (let* ((service-names (mapcar (lambda (s) (plist-get s :name)) stopped-services))
               (service-name (completing-read
                              (format "Start service for %s: " project-tag)
                              service-names nil t))
               (service (prodigy-find-service service-name)))
          (when service
            (prodigy-start-service service)
            (message "Started: %s" service-name)))))))

(defun my/projectile-stop-prodigy-process ()
  "Stop a prodigy process for the current project.
Shows completion for all running services tagged with the current project name."
  (interactive)
  (let* ((project-tag (project-processes-project-tag))
         (running-services (project-processes-running-services-with-tag project-tag)))
    (if (not project-tag)
        (message "Not in a projectile project")
      (if (not running-services)
          (message "No running services found for project: %s" project-tag)
        (let* ((service-names (mapcar (lambda (s) (plist-get s :name)) running-services))
               (service-name (completing-read
                              (format "Stop service for %s: " project-tag)
                              service-names nil t))
               (service (prodigy-find-service service-name)))
          (when service
            (prodigy-stop-service service)
            (message "Stopped: %s" service-name)))))))

;; Bulk operations for current project
(defun my/projectile-start-all-prodigy-services ()
  "Start all stopped prodigy services for the current project."
  (interactive)
  (let* ((project-tag (project-processes-project-tag))
         (stopped-services (project-processes-stopped-services-with-tag project-tag)))
    (if (not project-tag)
        (message "Not in a projectile project")
      (if (not stopped-services)
          (message "No stopped services found for project: %s" project-tag)
        (let ((started-services '()))
          (dolist (service stopped-services)
            (let ((service-name (plist-get service :name)))
              (prodigy-start-service service)
              (push service-name started-services)
              (message "Started: %s" service-name)))
          (message "Started %d services for %s: %s"
                   (length started-services)
                   project-tag
                   (string-join (reverse started-services) ", ")))))))

(defun my/projectile-stop-all-prodigy-services ()
  "Stop all running prodigy services for the current project."
  (interactive)
  (let* ((project-tag (project-processes-project-tag))
         (running-services (project-processes-running-services-with-tag project-tag)))
    (if (not project-tag)
        (message "Not in a projectile project")
      (if (not running-services)
          (message "No running services found for project: %s" project-tag)
        (let ((stopped-services '()))
          (dolist (service running-services)
            (let ((service-name (plist-get service :name)))
              (prodigy-stop-service service)
              (push service-name stopped-services)
              (message "Stopped: %s" service-name)))
          (message "Stopped %d services for %s: %s"
                   (length stopped-services)
                   project-tag
                   (string-join (reverse stopped-services) ", ")))))))

(defun my/projectile-restart-all-prodigy-services ()
  "Restart all prodigy services for the current project."
  (interactive)
  (let ((project-tag (project-processes-project-tag)))
    (if (not project-tag)
        (message "Not in a projectile project")
      (projectile-stop-all-prodigy-services)
      (run-with-timer 3 nil #'projectile-start-all-prodigy-services))))


(provide 'setup-projectile)
;;; setup-projectile.el ends here
