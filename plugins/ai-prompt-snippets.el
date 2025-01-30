;;; ai-prompt-snippets.el --- LLM Prompt Library -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: convenience, ai
;; URL: https://github.com/yourusername/ai-prompt-snippets

;;; Commentary:

;; A library for managing and using LLM prompts with preview and
;; copy/insert capabilities.

;;; Code:

(require 'transient)

(defgroup ai-prompt-snippets nil
  "Customization group for AI Prompt Library."
  :group 'ai)

(defcustom ai-prompt-snippets-directory (expand-file-name "ai-snippets" user-emacs-directory)
  "Directory containing prompt files."
  :type 'directory
  :group 'ai-prompt-snippets)

(defvar ai-prompt-snippets-prompts
  '(("objective 50/50"
     :description "Argue pros and cons"
     :content "# objective 50/50
BEFORE YOU ANSWER, I want you to write two detailed paragraphs, one arguing for each of these solutions - do not jump to conclusions, seriously consider both approaches.

After you finish, tell me wherer of these solutions is obviously better than the other and why. ")

    ("Summarize"
     :description "Summarize the discussion"
     :content "Please summarize the current discussion including what we just did, what did not work, which files were created/updated, what mistakes to avoid, any key insights/lessons we've learned, what problems/errors we are facing, ... and anything else a programmer might need to work productively on this project.

Write in a conversational yet informative tone, something like a README file that is information dense and without any fluff or noise. DO NOT include any assumptions or theories, just the facts.

I expect to see three concise paragraphs, written as if you were giving instructions to another programmer and this was ALL you could tell him.
")

    ("Code Review"
     :description "Review code with best practices"
     :content "Please review this code considering:

1. Code quality and best practices
2. Potential bugs or edge cases
3. Performance implications
4. Security considerations
5. Suggested improvements"))
  "Collection of predefined prompts.")

(defun ai-prompt-snippets--get-prompt-content (prompt-key)
  "Get content for PROMPT-KEY from `ai-prompt-snippets-prompts'."
  (plist-get (cdr (assoc prompt-key ai-prompt-snippets-prompts)) :content))

(defun ai-prompt-snippets--copy-to-clipboard (prompt-key)
  "Copy prompt content for PROMPT-KEY to clipboard."
  (when-let ((content (ai-prompt-snippets--get-prompt-content prompt-key)))
    (kill-new content)
    (message "Copied prompt '%s' to clipboard" prompt-key)))

(defun ai-prompt-snippets--insert-at-point (prompt-key)
  "Insert prompt content for PROMPT-KEY at point."
  (when-let ((content (ai-prompt-snippets--get-prompt-content prompt-key)))
    (insert content)
    (message "Inserted prompt '%s'" prompt-key)))

(defclass ai-prompt-snippets--preview (transient-suffix)
  ((prompt-key :initarg :prompt-key)
   (transient :initarg :transient)))


;; First, define the preview command
(defun ai-prompt-snippets--preview-cmd (prompt-key)
  "Display a preview of the prompt content for PROMPT-KEY."
  (interactive
   (list (completing-read "Prompt to preview: "
                          (mapcar #'car ai-prompt-snippets-prompts)
                          nil t)))
  (let ((content (ai-prompt-snippets--get-prompt-content prompt-key)))
    (with-help-window "*Prompt Preview*"
      (with-current-buffer standard-output
        (insert (format "Preview of prompt '%s':\n\n" prompt-key))
        (insert content)))))

(defclass ai-prompt-snippets--action-switch (transient-switch)
  ((action :initarg :action)
   (argument :initarg :argument))       ; Add the argument slot
  "Class used for action switches in the prompt menu.")

(defvar ai-prompt-snippets--action 'copy
  "Current action to perform on prompt selection (copy or insert).")

(cl-defmethod transient-infix-set ((obj ai-prompt-snippets--action-switch) value)
  "Set the action VALUE for OBJ and update the global action state."
  (when value
    (setq ai-prompt-snippets--action (oref obj action)))
  (oset obj value value))

(cl-defmethod transient-format-value ((obj ai-prompt-snippets--action-switch))
  "Format the switch display for OBJ."
  (let ((value (eq ai-prompt-snippets--action (oref obj action))))
    (concat
     (propertize "(" 'face 'transient-delimiter)
     (propertize (if value "*" " ") 'face 'transient-value)
     (propertize ")" 'face 'transient-delimiter))))

(cl-defmethod transient-infix-set ((obj ai-prompt-snippets--action-switch) value)
  "Set the action VALUE for OBJ and update the global action state."
  (when value
    (setq ai-prompt-snippets--action (oref obj action)))
  (oset obj value value))

(cl-defmethod transient-format-value ((obj ai-prompt-snippets--action-switch))
  "Format the switch display for OBJ."
  (let ((value (eq ai-prompt-snippets--action (oref obj action))))
    (concat
     (propertize "(" 'face 'transient-delimiter)
     (propertize (if value "*" " ") 'face 'transient-value)
     (propertize ")" 'face 'transient-delimiter))))

(transient-define-prefix ai-prompt-snippets-menu ()
  "Menu for managing and using LLM prompts."
  [:description
   (lambda ()
     (propertize "LLM Prompt Library" 'face 'transient-heading))

   ["Action"
    ("c" "Copy to clipboard"
     (lambda () (interactive)
       (setq ai-prompt-snippets--action 'copy)
       (message "AI Prompt Action: Copy")
       (transient-setup 'ai-prompt-snippets-menu))
     :class ai-prompt-snippets--action-switch
     :argument ""  ; Add empty argument
     :action 'copy)
    ("i" "Insert at point"
     (lambda () (interactive)
       (setq ai-prompt-snippets--action 'insert)
       (message "AI Prompt Action: Insert")
       (transient-setup 'ai-prompt-snippets-menu))
     :class ai-prompt-snippets--action-switch
     :argument ""  ; Add empty argument
     :action 'insert)]

   ["Prompts"
    ("d" "Debate (pros/cons)"
     (lambda ()
       (interactive)
       (pcase ai-prompt-snippets--action
         ('copy (ai-prompt-snippets--copy-to-clipboard "Debate"))
         ('insert (ai-prompt-snippets--insert-at-point "Debate")))
       (transient-quit-one)))

    ("s" "Summarize discussion"
     (lambda ()
       (interactive)
       (pcase ai-prompt-snippets--action
         ('copy (ai-prompt-snippets--copy-to-clipboard "Summarize"))
         ('insert (ai-prompt-snippets--insert-at-point "Summarize")))
       (transient-quit-one)))

    ("r" "Code review"
     (lambda ()
       (interactive)
       (pcase ai-prompt-snippets--action
         ('copy (ai-prompt-snippets--copy-to-clipboard "Code Review"))
         ('insert (ai-prompt-snippets--insert-at-point "Code Review")))
       (transient-quit-one)))]

   ["Other"
    ("p" "Preview prompt" ai-prompt-snippets--preview-cmd)
    ("RET" "Select from all prompts"
     (lambda ()
       (interactive)
       (let ((prompt-key (completing-read "Prompt: "
                                          (mapcar #'car ai-prompt-snippets-prompts)
                                          nil t)))
         (pcase ai-prompt-snippets--action
           ('copy (ai-prompt-snippets--copy-to-clipboard prompt-key))
           ('insert (ai-prompt-snippets--insert-at-point prompt-key))))))]])


(cl-defmethod transient-init-value ((obj ai-prompt-snippets--preview))
  "Initialize preview for OBJ."
  (oset obj value
        (ai-prompt-snippets--get-prompt-content (oref obj prompt-key))))

(cl-defmethod transient-format ((obj ai-prompt-snippets--preview))
  "Format the preview display for OBJ."
  (let* ((content (ai-prompt-snippets--get-prompt-content (oref obj prompt-key)))
         (wrapped (with-temp-buffer
                    (insert content)
                    (fill-region (point-min) (point-max) 60)
                    (buffer-string)))
         (preview (if (> (length wrapped) 300)
                      (concat (substring wrapped 0 297) "...")
                    wrapped)))
    (concat
     (propertize "Preview:\n" 'face 'transient-heading)
     (propertize preview 'face 'transient-value))))

(defun ai-prompt-snippets-copy (prompt-key)
  "Copy prompt PROMPT-KEY to clipboard."
  (interactive
   (list (completing-read "Prompt: "
                          (mapcar #'car ai-prompt-snippets-prompts)
                          nil t)))
  (ai-prompt-snippets--copy-to-clipboard prompt-key))

(defun ai-prompt-snippets-insert (prompt-key)
  "Insert prompt PROMPT-KEY at point."
  (interactive
   (list (completing-read "Prompt: "
                          (mapcar #'car ai-prompt-snippets-prompts)
                          nil t)))
  (ai-prompt-snippets--insert-at-point prompt-key))

;; Alternative completion-based interface
;;;###autoload
(defun ai-prompt-snippets-quick-access ()
  "Quick access to prompt snippets with completion."
  (interactive)
  (let* ((choices (mapcar (lambda (prompt)
                            (cons (format "%s - %s"
                                          (car prompt)
                                          (plist-get (cdr prompt) :description))
                                  (car prompt)))
                          ai-prompt-snippets-prompts))
         (choice (completing-read "Prompt: " choices nil t))
         (prompt-key (cdr (assoc choice choices)))
         (action (completing-read "Action: "
                                  '("Copy to clipboard" "Insert at point")
                                  nil t)))
    (pcase action
      ("Copy to clipboard" (ai-prompt-snippets--copy-to-clipboard prompt-key))
      ("Insert at point" (ai-prompt-snippets--insert-at-point prompt-key)))))

;; Storage and loading functions
(defun ai-prompt-snippets-save-prompt (name description content &optional category)
  "Save a new prompt with NAME, DESCRIPTION, and CONTENT.
Optionally specify a CATEGORY for organization."
  (interactive "sPrompt name: \nsDescription: \nsContent: \nsCategory (optional): ")
  (let ((prompt (list name
                      :description description
                      :content content
                      :category (or category "General"))))
    (push prompt ai-prompt-snippets-prompts)
    (ai-prompt-snippets--save-prompts)))

(defun ai-prompt-snippets--save-prompts ()
  "Save prompt snippets to disk."
  (unless (file-exists-p ai-prompt-snippets-directory)
    (make-directory ai-prompt-snippets-directory t))
  (with-temp-file (expand-file-name "prompts.el" ai-prompt-snippets-directory)
    (print ai-prompt-snippets-prompts (current-buffer))))

(defun ai-prompt-snippets--load-prompts ()
  "Load prompt snippets from disk."
  (let ((file (expand-file-name "prompts.el" ai-prompt-snippets-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (setq ai-prompt-snippets-prompts (read (current-buffer)))))))

;; Load prompts when package is loaded
(ai-prompt-snippets--load-prompts)

;;;###autoload
(defun ai-prompt-snippets ()
  "Show the main prompt library interface."
  (interactive)
  (transient-setup 'ai-prompt-snippets-menu))

(provide 'ai-prompt-snippets)
;;; ai-prompt-snippets.el ends here
