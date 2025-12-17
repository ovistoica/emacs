;;; fast-startup.el --- Restore startup optimizations -*- lexical-binding: t -*-
;;
;; This file restores the settings that were optimized in early-init.el
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

;; Report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Restore optimized settings after idle (5 seconds)
(run-with-idle-timer
  5 nil
  (lambda ()
    ;; Restore gc-cons-threshold to a reasonable value (20 MB)
    (setq gc-cons-threshold (* 1024 1024 20))
    ;; Restore file-name-handler-alist
    (setq file-name-handler-alist file-name-handler-alist-original)
    (makunbound 'file-name-handler-alist-original)))

(provide 'fast-startup)
;;; fast-startup.el ends here
