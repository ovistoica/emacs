;;; ai-project-agent.el --- Utilities to have a project AI agent

;; Version: 1.0.0
;; Author: Ovi Stoica <ovidiu.stoica1094@gmail.com>
;; Url: https://github.com/ovistoica/emacs-ai
;; Keywords: convenience, project, ai, emacs

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package allows you to easily work with AI agents.  It tries to emulate
;; many of the useful functionalities from modern editors like Cursor or Windsurf.

;; It provides an AI chat window and helpful commands around it

;;; Code:

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

(defun ai-project-agent--detect-project-context ()
  "Detect the dominant project file and return context information."
  (when-let ((project-root (project-root (project-current))))
    (let* ((package-files '(("package.json" . "JavaScript/Node.js")
                            ("Cargo.toml" . "Rust")
                            ("go.mod" . "Go")
                            ("requirements.txt" . "Python")
                            ("composer.json" . "PHP")
                            ("Gemfile" . "Ruby")
                            ("pom.xml" . "Java")
                            ("build.gradle" . "Java")))
           (found-file (seq-find (lambda (file)
                                   (file-exists-p (expand-file-name (car file) project-root)))
                                 package-files)))
      (when found-file
        (cons (car found-file) (cdr found-file))))))


(defun ai-project-agent--enhance-prompt (buffer)
  "Add project context to the AI prompt in BUFFER."
  (with-current-buffer buffer
    (when-let* ((context (ai-project-agent--detect-project-context))
                (file (car context))
                (lang (cdr context)))
      (gptel-request
          buffer
        (format "You are a helpful AI assistant with expertise in %s development. The current project uses %s as its package manager. Please provide relevant advice and responses in this context."
                lang file)))))

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
          (if (bobp) (gptel-prompt-prefix-string))
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
