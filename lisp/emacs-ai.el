;;; emacs-ai.el --- Minor mode for interacting with AI agents.

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



(defun list-project (&optional file-regex)
  (if-let* ((proj (project-current))
            (root (project-root proj))
            (default-regex "\\.\\(el\\|clj\\|cljs\\|cljc\\|js\\|jsx\\|ts\\|tsx\\|rb\\|py\\|go\\|rs\\|cpp\\|c\\|h\\|hpp\\|java\\|php\\)$")
            (regex (or file-regex default-regex))
            (files (project-files proj)))
      (let ((matching-files
             (cl-remove-if-not
              (lambda (file)
                (string-match-p regex (file-relative-name file root)))
              files)))
        (concat "Project root: " (abbreviate-file-name root) "\n"
                "Files:\n"
                (mapconcat
                 (lambda (file)
                   (concat "- " (file-relative-name file root)))
                 matching-files
                 "\n")))
    "No project found or no matching files."))

(defun emacs-ai-get-project-root ()
  "Get the root directory of the current project.
Returns the absolute path to the project root or nil if not in a project."
  (when-let* ((proj (project-current))
              (root (project-root proj)))
    (expand-file-name root)))



(use-package gptel
  :straight (:host github :repo "karthink/gptel" :branch "feature-tool-use")
  :after project
  :defines
  gptel-make-anthropic
  gptel-api-key
  :preface
  (defun os/prompt-file (filename)
    (expand-file-name (concat "prompts/" filename) user-emacs-directory))
  (defun os/gptel-load-directive-from-markdown (file)
    "Load a gptel directive from a markdown FILE.
Returns a cons of (name . directive) where name is derived from filename
and directive is the content of the file."
    (condition-case err
        (let ((max-specpdl-size (* 10 max-specpdl-size)) ; Increase recursion limit
              (max-lisp-eval-depth (* 10 max-lisp-eval-depth))
              (large-file-warning-threshold nil) ; Disable large file warning
              (gc-cons-threshold (* 100 1024 1024))) ; 100MB for GC threshold
          (with-temp-buffer
            ;; Temporarily increase buffer size limit for this operation
            (let ((enable-local-variables nil)
                  (buffer-read-only nil)
                  (buffer-file-name nil)
                  (max-mini-window-height 0.5))
              (insert-file-contents file)
              (let* ((filename (file-name-nondirectory file))
                     (name (intern (car (split-string filename "\\.md"))))
                     (content (buffer-substring-no-properties
                               (point-min)
                               (point-max))))
                (cons name (string-trim content))))))
      (error
       (message "Error loading directive from %s: %s"
                file (error-message-string err))
       nil)))

  (defun os/gptel-load-all-markdown-directives (directory)
    "Load all markdown files from DIRECTORY as gptel directives.
Returns a list of cons cells (name . directive) for each .md file."
    (when (file-directory-p directory)
      (let ((markdown-files (directory-files directory t "\\.md$")))
        (delq nil
              (mapcar #'os/gptel-load-directive-from-markdown markdown-files)))))
  (gptel-make-tool
   :function (lambda (url)
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char (point-min)) (forward-paragraph)
                 (let ((dom (libxml-parse-html-region (point) (point-max))))
                   (run-at-time 0 nil #'kill-buffer (current-buffer))
                   (with-temp-buffer
                     (shr-insert-document dom)
                     (buffer-substring-no-properties (point-min) (point-max))))))
   :name "read_url"
   :description "Fetch and read the contents of a URL"
   :args (list '(:name "url"
                       :type "string"
                       :description "The URL to read"))
   :category "web")

  (gptel-make-tool
   :function
   (lambda ()
     (if-let* ((proj (project-current))
               (root (project-root proj)))
         (let ((root-path (expand-file-name root)))
           (format "Project root directory: %s\nDirectory exists: %s\nIs directory: %s"
                   root-path
                   (file-exists-p root-path)
                   (file-directory-p root-path)))
       "No project found in the current context."))
   :name "get_project_root"
   :description "Get the root directory of the current project. This is useful for understanding the project structure and performing operations relative to the project root."
   :args nil
   :category "project")

  (gptel-make-tool
   :function
   (lambda ()
     (if-let* ((proj (project-current))
               (root (project-root proj)))
         (let ((root-path (expand-file-name root)))
           (format "Project root directory: %s\nDirectory exists: %s\nIs directory: %s"
                   root-path
                   (file-exists-p root-path)
                   (file-directory-p root-path)))
       "No project found in the current context."))
   :name "get_project_root"
   :description "Get the root directory of the current project. This is useful for understanding the project structure and performing operations relative to the project root."
   :args nil
   :category "project")

  (gptel-make-tool
   :function (lambda ()
               "End the call"
               (message "Ending the call succesfully")
               "Ended call succesfully")
   :name "end_call"
   :description "End the call after closing conversation with the customer"
   :category "vodafone")

  ;; Message buffer logging tool
  (gptel-make-tool
   :function (lambda (text)
               (message "%s" text)
               (format "Message sent: %s" text))
   :name "echo_message"
   :description "Send a message to the *Messages* buffer"
   :args (list '(:name "text"
                       :type "string"
                       :description "The text to send to the messages buffer"))
   :category "emacs")

  ;; buffer retrieval tool
  (gptel-make-tool
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "Error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
                       :type "string"
                       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")


  (gptel-make-tool
   :function (lambda (directory)
	       (mapconcat #'identity
                          (directory-files directory)
                          "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
	               :type "string"
	               :description "The path to the directory to list"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (parent name)
               (condition-case nil
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
	               :type "string"
	               :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
	               :type "string"
	               :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
	               :type "string"
	               :description "The directory where to create the file")
               '(:name "filename"
	               :type "string"
	               :description "The name of the file to create")
               '(:name "content"
	               :type "string"
	               :description "The content to write to the file"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (filepath)
	       (with-temp-buffer
	         (insert-file-contents (expand-file-name filepath))
	         (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
	               :type "string"
	               :description "Path to the file to read.  Supports relative paths and ~."))
   :category "filesystem")
  (gptel-make-tool
   :function
   (lambda (&optional file-regex)
     (if-let* ((proj (project-current))
               (root (project-root proj))
               (default-regex "\\.\\(el\\|clj\\|cljs\\|cljc\\|js\\|jsx\\|ts\\|tsx\\|rb\\|py\\|go\\|rs\\|cpp\\|c\\|h\\|hpp\\|java\\|php\\)$")
               (regex (or file-regex default-regex))
               (files (project-files proj)))
         (let ((matching-files
                (cl-remove-if-not
                 (lambda (file)
                   (string-match-p regex (file-relative-name file root)))
                 files)))
           (concat "Project root: " (abbreviate-file-name root) "\n"
                   "Files:\n"
                   (mapconcat
                    (lambda (file)
                      (concat "- " (file-relative-name file root)))
                    matching-files
                    "\n")))
       "No project found or no matching files."))
   :name "list_project_files"
   :description "List programming files in the current project directory. Use this function to understand which files you want to read so you can better understand the request from the user."
   :args (list '(:name "file_regex"
                       :type "string"
                       :description "Optional regex pattern to filter files (e.g., \"\\.py$\" for Python files). If not provided, lists common programming files."))
   :category "project")

  :init
  (setq gptel-default-mode 'org-mode)   ; Use org-mode as the default
  :bind (("C-c <enter>" . gptel-send)
         ("C-c RET" . gptel-send)
         ("C-c C-<enter>" . gptel-menu)
         ("C-c C-a" . emacs-ai-toggle-ai-pannel))
  :config
  (setq gptel-api-key os-secret-openai-key)
  (gptel-make-gemini "Gemini" :key os-secret-google-gemini-api-key :stream t)
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key os-secret-anthropic-key)
        gptel-model 'claude-3-5-sonnet-20241022
        gptel-temperature 0.7
        ;; Configure the chat UI
        gptel-window-select t           ; Select the window after creation
        gptel-window-side 'right        ; Display on the right side
        gptel-window-width 80           ; Set window width
        )
  (setq gptel-directives
        (let ((markdown-directives (os/gptel-load-all-markdown-directives (expand-file-name "prompts" user-emacs-directory))))
          `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications.  Speak in specific, topic relevant terminology.  Do NOT hedge or qualify.  Speak directly and be willing to make creative guesses.

Explain your reasoning.  if you don’t know, say you don’t know.  Be willing to reference less reputable sources for ideas.  If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.

 Never apologize.  Ask questions when unsure.")
            (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
            (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
            (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")

            (explain . "Explain what this code does to a novice programmer.")
            (tutor . "You are a tutor and domain expert in the domain of my questions.  You will lead me to discover the answer myself by providing hints.  Your instructions are as follows:
- If the question or notation is not clear to you, ask for clarifying details.
- At first your hints should be general and vague.
- If I fail to make progress, provide more explicit hints.
- Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it.
- If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")
            ,@markdown-directives
            ))))

(use-package ai-project-agent
  :after (gptel flycheck)
  :bind (("C-c c a" . ai-project-agent-toggle-panel)
         ("C-c c d" . ai-project-agent-clear-panel)
         ("C-c c c" . gptel-add)
         ("C-c c l" . ai-project-agent-send-lint-feedback)
         ("C-c c RET" . ai-project-agent-send)))

(use-package corsair
  :straight (:host github
                   :repo "rob137/Corsair"
                   :files ("corsair.el")) ; Load the specific file
  :defer t                                ; Optional: Load lazily
  :after gptel                          ; Ensure gptel is loaded before corsair
  )

(use-package copilot
  :defines
  copilot-max-char
  :commands
  copilot-mode
  :preface
  (defun os/activate-copilot ()
    (if (or (> (buffer-size) 100000)
            (string-prefix-p "*temp*-" (buffer-name)))
        ;; Or don't even warn to get rid of it.
        (message "Buffer size exceeds copilot max char limit or buffer is temporary. Copilot will not be activated.")
      (copilot-mode)))
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :config
  (setq copilot-enable-predicates nil
        warning-suppress-types '((copilot copilot--infer-indentation-offset)))
  (add-to-list 'copilot-major-mode-alist '("tsx-ts" . "typescriptreact"))
  (add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
  (add-to-list 'copilot-indentation-alist '(tsx-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(typescript-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojurec-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojurescript-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(css-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(json-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(dockerfile-mode 2))
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)
              ("S-C-TAB" . 'copilot-accept-completion-by-word)
              ("S-C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY" os-secret-anthropic-key)
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(require 'project)






(provide 'emacs-ai)
;;; emacs-ai.el ends here

;; This is not a literate config tangled from an Org-mode document! So I include
;; some file-specific settings to make it easier to parse. Specifically, the
;; outline that you see in this document is represented in the Lisp files as
;; Org-style collapsible outline headings. See [[*OUTLINE MODE][Outline Mode]].

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
