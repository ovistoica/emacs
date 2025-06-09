;; gptel is a LLM chat client for Emacs, with support for multiple models & backends.


(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :bind (("C-c <enter>" . gptel-send)
         ("C-c RET" . gptel-send)
         ("C-c C-<enter>" . gptel-menu))
  :init
  (setq gptel-default-mode 'org-mode)   ; Use org-mode as the default
  :config
  (require 'ai-project-agent)
  (require 'private)

  (add-hook 'gptel-mode-hook
            (lambda ()
              (when (and (derived-mode-p 'org-mode)
                         (bound-and-true-p org-indent-mode))
                (org-indent-mode -1))))


  (setq gptel-api-key my/secret-openai-key)
  (gptel-make-gemini "Gemini" :key my/secret-google-gemini-api-key :stream t)
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key my/secret-deepseek-api-key
    :models '(deepseek-chat deepseek-reasoner))

  (setq gptel-backend  (gptel-make-anthropic "Claude"
                         :stream t
                         :key my/secret-anthropic-key)
        gptel-model 'claude-sonnet-4-20250514
        gptel-temperature 0.7

        ;; Configure the chat UI
        gptel-window-select t           ; Select the window after creation
        gptel-window-side 'right        ; Display on the right side
        gptel-window-width 80           ; Set window width

        )
  (setq gptel-directives (my/gptel-load-all-markdown-directives (expand-file-name "ai-prompts" user-emacs-directory))))

(use-package mcp
  :ensure t
  :after gptel
  :custom
  (mcp-hub-servers
   `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/ovistoica/workspace/shipclojure-datom/")))
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ("clj-prj" . (:command "/bin/sh"
                            :args ("-c"
                                   ,(concat "cd /Users/ovistoica/workspace/shipclojure-datom && "
                                           "PATH=/opt/homebrew/bin:$PATH && "
                                           "clojure -X:mcp :port 7888"))))))
  :config
  (require 'mcp-hub)
  (require 'gptel-integrations))

(use-package ai-project-agent
  :ensure nil
  :bind (("C-c C-a" . ai-project-agent-toggle-panel)))


(defun my/prompt-file (filename)
  (expand-file-name (concat "prompts/" filename) user-emacs-directory))

(defun my/gptel-load-directive-from-markdown (file)
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

(defun my/gptel-load-all-markdown-directives (directory)
  "Load all markdown files from DIRECTORY as gptel directives.
Returns a list of cons cells (name . directive) for each .md file."
  (when (file-directory-p directory)
    (let ((markdown-files (directory-files directory t "\\.md$")))
      (delq nil
            (mapcar #'my/gptel-load-directive-from-markdown markdown-files)))))
