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

(defun causal-run (name)
  "Run NAME process."
  (let ((buf (causal--create-process-buffer name)))
    (async-shell-command (format "causal %s" name) buf)))

;;;###autoload
(defun causal-run-frontend ()
  "Start just causal frontend."
  (interactive)
  (causal-run "frontend"))

;;;###autoload
(defun causal-run-cow ()
  "Start just causal frontend."
  (interactive)
  (causal-run "cow --verbose"))

;;;###autoload
(defun causal-run-backend ()
  "Start just causal frontend."
  (interactive)
  (causal-run "backend --verbose"))

;;;###autoload
(defun causal-run-all ()
  "Run all Causal commands in parallel."
  (interactive)
  (causal-run-cow)
  (causal-run-backend)
  (causal-run-frontend))

(provide 'causal-dev)
