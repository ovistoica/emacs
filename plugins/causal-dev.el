;;; causal-dev.el --- Useful utils for causal  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ovi Stoica

;; Keywords: processes, tools
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This package provides functionality for the Causal project.

;;; Code:

(require 'cl-lib)

(defmacro with-location (directory &rest body)
  "Execute BODY with DIRECTORY as the default directory.
DIRECTORY should be a string specifying the path to the directory.
The original `default-directory' is restored after BODY executes."
  (declare (indent 1) (debug t))
  `(let ((default-directory (expand-file-name ,directory)))
     ,@body))



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
(defun causal-run (cmd)
  "Execute a causal cli CMD."
  (interactive "sEnter causal command to run: ")
  (let ((compilation-buffer-name-function (lambda (_mode)
                                            (format "%s" cmd))))
    (compile (format "%s" cmd))))

(defun causal-run-frontend-sandbox ()
  (interactive)
  "Run causal in sandbox mode"
  (causal-run "causal frontend --environment sandbox-01"))

;;;###autoload
(defun causal-run-frontend ()
  "Start just causal frontend."
  (interactive)
  (with-location "~/workspace/causal/workspace/"
    (causal-run "SKIP_YARN=true causal frontend")))

(defun causal-install ()
  "Install causal yarn deps"
  (interactive)
  (with-location "~/workspace/causal/workspace/"
    (causal-run "yarn")))
;;;###autoload
(defun causal-run-frontend-federated ()
  "Run frontend in federated mode."
  (interactive)
  (with-location "~/workspace/causal/workspace/"
    (causal-run "MODULE_FEDERATION=true RUNNING_IN_LUCANET=true PORT=4000 yarn workspace @causal/frontend dev")))

;;;###autoload
(defun causal-run-cow ()
  "Start just causal frontend."
  (interactive)
  (causal-run "causal cow --verbose"))

;;;###autoload
(defun causal-run-backend ()
  "Start just causal frontend."
  (interactive)
  (causal-run "causal backend --verbose"))

;;;###autoload
(defun causal-run-all ()
  "Run all Causal commands in parallel."
  (interactive)
  (causal-run-cow)
  (causal-run-backend)
  (causal-run-frontend))

(provide 'causal-dev)
;;; causal-dev.el ends here
