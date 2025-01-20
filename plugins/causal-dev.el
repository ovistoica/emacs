;;; causal-dev.el --- Useful utils for causal  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ovi Stoica

;; Keywords: processes, tools
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This package provides functionality for the Causal project.

;;; Code:

(require 'cl-lib)

(defgroup causal nil
  "Run Causal commands."
  :group 'tools
  :prefix "causal-")

(defcustom causal-buffer-names
  '(("frontend" . "causal frontend")
    ("backend" . "causal backend")
    ("cow" . "causal cow"))
  "Alist of buffer names and their corresponding shell commands."
  :type '(alist :key-type string :value-type string)
  :group 'causal)

(defvar causal--process-buffers nil
  "List of buffers created for parallel processes.")

(defun causal--create-process-buffer (name)
  "Create a uniquely named buffer for process NAME."
  (generate-new-buffer (format "*Causal-%s*" name)))

(defun causal--cleanup-buffers ()
  "Clean up process buffers when they're finished."
  (dolist (buf causal--process-buffers)
    (when (and (buffer-live-p buf)
               (get-buffer-process buf)
               (not (process-live-p (get-buffer-process buf))))
      (with-current-buffer buf
        (view-mode 1)))))

(defun causal-kill-processes ()
  "Kill all running Causal processes."
  (interactive)
  (dolist (buf causal--process-buffers)
    (when (and (buffer-live-p buf)
               (get-buffer-process buf)
               (process-live-p (get-buffer-process buf)))
      (kill-process (get-buffer-process buf))
      (message "Killed process in buffer: %s" (buffer-name buf)))))

;;;###autoload
(defun causal-run-all ()
  "Run all Causal commands in parallel."
  (interactive)
  ;; Reset the process buffers list
  (setq causal--process-buffers nil)

  ;; Create and store buffers for each process
  (dolist (cmd-pair causal-buffer-names)
    (let* ((name (car cmd-pair))
           (command (cdr cmd-pair))
           (buf (causal--create-process-buffer name)))
      (push buf causal--process-buffers)

      ;; Run the async process
      (with-current-buffer buf
        (async-shell-command command buf (format "*Causal-%s-error*" name))

        ;; Set up process sentinel
        (set-process-sentinel
         (get-buffer-process buf)
         (lambda (process event)
           (when (string-match-p "finished\\|exited" event)
             (causal--cleanup-buffers)))))))

  ;; Display all buffers
  (delete-other-windows)
  (when causal--process-buffers
    (let* ((window-count (length causal--process-buffers))
           (split-height (/ (frame-height) window-count)))
      (dolist (buf causal--process-buffers)
        (split-window-below split-height)
        (other-window 1)
        (switch-to-buffer buf)))
    (other-window 1)))

(provide 'causal-dev)
