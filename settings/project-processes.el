;; project-processes.el

;; Use prodigy with projectile to start processes related to that project upon visiting

(require 'projectile)
(require 'prodigy)

;; Core helper functions
(defun project-processes-project-tag ()
  "Get the current project name as a tag for prodigy services.
Returns the project name as a symbol if in a projectile project, nil otherwise."
  (when (and (fboundp 'projectile-project-p) (projectile-project-p))
    (intern (projectile-project-name))))

(defun project-processes-services-with-tag (tag)
  "Return all prodigy services that have the specified TAG."
  (when tag
    (cl-remove-if-not
     (lambda (service)
       (member tag (plist-get service :tags)))
     prodigy-services)))

(defun project-processes-running-services-with-tag (tag)
  "Return all running prodigy services that have the specified TAG."
  (cl-remove-if-not
   #'prodigy-service-started-p
   (project-processes-services-with-tag tag)))

(defun project-processes-stopped-services-with-tag (tag)
  "Return all stopped prodigy services that have the specified TAG."
  (cl-remove-if
   #'prodigy-service-started-p
   (project-processes-services-with-tag tag)))

;; Project switching hooks
(defvar project-processes-previous-project-tag nil
  "Track the previous project tag for service management.")

(defun project-processes-track-project-state ()
  "Track project state for service management."
  (setq project-processes-previous-project-tag (project-processes-project-tag)))

(defun project-processes-project-switch-hook ()
  "Hook to run when switching projects - auto-start/stop services based on project tags."
  (let ((current-project-tag (project-processes-project-tag)))
    (cond
     ;; Switched TO a project with services
     ((and current-project-tag
           (project-processes-services-with-tag current-project-tag))
      (message "Switched to project %s - starting services..." current-project-tag)
      (run-with-timer 1 nil #'my/projectile-start-all-prodigy-services))

     ;; Switched AWAY from a project that had services running
     ((and project-processes-previous-project-tag
           (project-processes-running-services-with-tag project-processes-previous-project-tag))
      (message "Left project %s - stopping services..." project-processes-previous-project-tag)
      (let ((project-tag project-processes-previous-project-tag))
        (dolist (service (project-processes-running-services-with-tag project-tag))
          (prodigy-stop-service service)
          (message "Stopped: %s" (plist-get service :name))))))))

;; File visit hook (alternative approach)
(defun project-processes-file-visit-hook ()
  "Hook to run when visiting files - auto-start services if not running."
  (let* ((project-tag (project-processes-project-tag))
         (project-services (project-processes-services-with-tag project-tag))
         (stopped-services (project-processes-stopped-services-with-tag project-tag)))
    (when (and project-tag
               project-services
               stopped-services
               ;; Only auto-start if no services are running for this project
               (not (project-processes-running-services-with-tag project-tag)))
      (message "Project %s detected - starting services..." project-tag)
      (run-with-timer 2 nil #'my/projectile-start-all-prodigy-services))))

;; Utility functions for inspection
(defun project-processes-list-project-services ()
  "List all prodigy services for the current project."
  (interactive)
  (let* ((project-tag (project-processes-project-tag))
         (services (project-processes-services-with-tag project-tag)))
    (if (not project-tag)
        (message "Not in a projectile project")
      (if (not services)
          (message "No services found for project: %s" project-tag)
        (let ((running-count 0)
              (stopped-count 0))
          (message "Services for project %s:" project-tag)
          (dolist (service services)
            (let ((name (plist-get service :name))
                  (status (if (prodigy-service-started-p service) "RUNNING" "STOPPED")))
              (if (prodigy-service-started-p service)
                  (cl-incf running-count)
                (cl-incf stopped-count))
              (message "  %s: %s" name status)))
          (message "Total: %d services (%d running, %d stopped)"
                   (length services) running-count stopped-count))))))


(provide 'project-processes)
