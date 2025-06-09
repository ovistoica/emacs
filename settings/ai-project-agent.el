;;; ai-project-agent.el --- Utilities to have a project AI agent

;; Version: 1.0.0
;; Author: Ovi Stoica <ovidiu.stoica1094@gmail.com>
;; Url: https://github.com/ovistoica/emacs
;; Keywords: convenience, project, ai, emacs
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package allows you to easily work with AI agents.  It tries to emulate
;; many of the useful functionalities from modern editors like Cursor or Windsurf.

;; It provides an AI chat window and helpful commands around it

;;; Code:
(require 'flycheck)
(require 'gptel)

(defvar-local ai-project-agent-buffer nil
  "Buffer for AI interaction in the current project.")

(defcustom ai-project-agent-window-width 80
  "Width of the AI side panel window."
  :type 'integer
  :group 'ai-project-agent)

(defun ai-project-agent-buffer-name ()
  "Generate the AI buffer name for the current project."
  (if-let ((project (project-current)))
      (format "*AI: %s*" (project-name project))
    "*AI*"))

(defun ai-project-agent-get-or-create-buffer ()
  "Get or create an AI buffer for the current project."
  (let ((buffer-name (ai-project-agent-buffer-name)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (cond                         ;Set major mode
           ((eq major-mode gptel-default-mode))
           ((eq gptel-default-mode 'text-mode)
            (text-mode)
            (visual-line-mode 1))
           (t (funcall gptel-default-mode)))
          (gptel--sanitize-model :backend (default-value 'gptel-backend)
                                 :model (default-value 'gptel-model)
                                 :shoosh nil)
          (unless gptel-mode (gptel-mode 1))
          (goto-char (point-max))
          (skip-chars-backward "\t\r\n")
          (when (bobp)
            (progn
              (gptel-prompt-prefix-string)))
          (current-buffer)))))


(defun ai-project-agent-display-buffer (buffer)
  "Display BUFFER in a side window."
  (display-buffer
   buffer
   `(display-buffer-in-side-window
     (side . right)
     (slot . 1)
     (window-width . ,ai-project-agent-window-width)
     (preserve-size . (t . nil)))))

(defun ai-project-agent-toggle-panel ()
  "Toggle the AI panel for the current project."
  (interactive)
  (if-let ((window (get-buffer-window (ai-project-agent-buffer-name))))
      (delete-window window)
    (let ((buffer (ai-project-agent-get-or-create-buffer)))
      (ai-project-agent-display-buffer buffer)
      (select-window (get-buffer-window buffer)))))


(defun ai-project-agent-send-lint-feedback ()
  "Send flycheck error at point to AI agent for feedback."
  (interactive)
  (let ((original-buffer-name (current-buffer)))
    (when-let* ((errors (flycheck-overlay-errors-at (point)))
                (error (car errors))
                (buffer (ai-project-agent-get-or-create-buffer))
                (context-start (save-excursion
                                 (forward-line -100)
                                 (point)))
                (context-end (save-excursion
                               (forward-line 100)
                               (point)))
                (surrounding-code (buffer-substring-no-properties context-start context-end))
                (code (if (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))
                        (when-let ((defun-bounds (bounds-of-thing-at-point 'defun)))
                          (buffer-substring-no-properties
                           (car defun-bounds)
                           (cdr defun-bounds))))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (format "\nPlease help me fix this error.\nFile: %s\nCode:\n#+begin_src\n%s\n#+end_src\nError from linter: \n#+begin_src\n%s\n#+end_src\n\nSurrounding context:\n#+begin_src\n%s\n#+end_src\n"
                        original-buffer-name
                        (or code "No code context found")
                        (flycheck-error-message error)
                        surrounding-code))
        (gptel-send))
      (ai-project-agent-display-buffer buffer))))


(defun ai-project-agent-send ()
  "Send query to AI agent with selected text if any."
  (interactive)
  (let* ((buffer (ai-project-agent-get-or-create-buffer))
         (selection (when (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning)
                       (region-end))))
         (instruction (read-string "Instruction: " nil nil)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\nInstruction: " instruction "\n")
      (when selection
        (insert "\nSelected text:\n" selection "\n"))
      (gptel-send))
    (ai-project-agent-display-buffer buffer)))

(defun ai-project-agent-clear-panel ()
  "Clear ai agent panel for current project."
  (interactive)
  (when-let ((buffer (get-buffer (ai-project-agent-buffer-name))))
    (with-current-buffer buffer
      (erase-buffer))))


(with-eval-after-load 'gptel-transient
  ;; Add new menu item to gptel-menu
  (transient-append-suffix 'gptel-menu '(1 2 "k")
    '("p" "Project AI buffer" "p"))

  (define-advice gptel--suffix-send (:filter-args (args) project-buffer)
    "When project buffer option is selected, direct response there."
    (when (member "p" (car args))
      (let ((buffer (ai-project-agent-get-or-create-buffer)))
        (with-current-buffer buffer
          (goto-char (point-max)))
        (setcar args (cons (concat "b" (buffer-name buffer)) (car args)))))
    args))

(provide 'ai-project-agent)
;;; ai-project-agent.el ends here
