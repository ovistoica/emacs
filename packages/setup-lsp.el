;;; setup-lsp --- Config for LSP providers -*- lexical-binding: t -*-

;;; Commentary:

;;; I use LSP mode instead of eglot as it supports multiple LSP providers in the
;;; same buffer - See more comparisons of LSP here:
;;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;;;
;;; Most of the features of lsp-mode are silenced to match the emacs way.
;;;
;;; lsp-booster is used to convert json packets to native elisp bytecode so
;;; emacs can offer faster suggestions.

;;; Code:

(require 'dash)

(declare-function lsp "lsp-mode" (&optional arg))
(declare-function lsp-session "lsp-mode")
(declare-function lsp--session-workspaces "lsp-mode" (session))
(declare-function lsp--workspace-client "lsp-mode" (workspace))
(declare-function lsp--client-server-id "lsp-mode" (client))
(declare-function lsp-workspace-restart "lsp-mode" (workspace))
(declare-function lsp-find-workspace "lsp-mode" (server-id &optional file-name))
(declare-function lsp-find-session-folder "lsp-mode" (session file-name))
(declare-function lsp-session-folder->servers "lsp-mode" (session))
(declare-function lsp-request "lsp-mode" (method params &rest args))
(declare-function lsp-register-client "lsp-mode" (client))
(declare-function lsp-stdio-connection "lsp-mode" (command &optional test-command))
(declare-function make-lsp-client "lsp-mode")
(declare-function url-unhex-string "url-util" (str &optional allow-newlines))
(defvar lsp-clojure-library-dirs)
(defvar lsp-clojure-workspace-cache-dir)

(use-package lsp-mode
  :preface
  (defun my/lsp-after-local-variables ()
    "Start lsp in this buffer once its file- and dir-local variables are set.
Mode hooks run before dir-locals are applied, so starting lsp from them
would ignore a project's `my/clojure-language-server'."
    (add-hook 'hack-local-variables-hook #'lsp nil t))
  :hook (((clojure-mode clojurescript-mode clojurec-mode edn-mode)
          . my/lsp-after-local-variables)
         (python-mode . lsp)
         ;; TypeScript modes
         (typescript-mode . lsp)
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         ;; JavaScript modes
         (js-mode . lsp)
         (js2-mode . lsp)
         (js-ts-mode . lsp)
         ;; Java
         (java-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :diminish " lsp"

  :bind ((:map lsp-mode-map
               ("s-l w l" . lsp-workspace-show-log)))

  :custom
  (lsp-completion-provider :none)  ;; Skip company-mode
  :init
  (setq lsp-headerline-breadcrumb-enable nil) ;; Don't need file path in my buffer
  (setq lsp-lens-enable t) ;; Show reference and test counts
  (setq lsp-enable-indentation nil) ;; use clojure-mode indentation
  (setq lsp-eldoc-enable-hover nil) ;; use CIDER eldoc
  (setq lsp-modeline-code-actions-enable nil) ;; Don't clutter modeline
  (setq lsp-modeline-diagnostics-enable nil) ;; Don't clutter modeline, jeez
  (setq lsp-enable-symbol-highlighting nil) ;; Don't highlight symbol - s l a h for when needed

  (setq lsp-apply-edits-after-file-operations nil) ;; Disable broken lsp feature: https://github.com/clojure-lsp/clojure-lsp/issues/1813

  ;; I'm working with dirs that have has ~1k legitimate source directories -
  ;; raise the threshold to suppress the confirmation prompt without disabling
  ;; watchers
  (setq lsp-file-watch-threshold 2000)

  ;; Shut a server down once its last buffer is closed.  With one server per
  ;; git worktree, idle servers otherwise pile up until Emacs exits.
  (setq lsp-keep-workspace-alive nil)

  (setq lsp-signature-doc-lines 1)      ; Don't raise the echo area. It's distracting

  ;; semgrep-ls is not installed and not needed - silence the prompt
  ;; ts-ls is installed as a fallback but disabled: we prefer tsgo, the
  ;; Go-based TypeScript server (@typescript/native-preview).  To roll back
  ;; to ts-ls, remove it from this list (typescript@5 + ts-ls live in
  ;; `lsp-server-install-dir').
  (setq lsp-disabled-clients '(semgrep-ls ts-ls))


  ;; To consider
  ;;
  ;; (setq lsp-enable-completion-at-point nil) ;; CIDER vs LSP?
  ;; (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)

  :config
  (advice-add 'lsp--info :around #'my/silence-some-lsp-info-messages)
  (add-hook 'lsp-completion-mode-hook 'my/use-lsp-completion-only-as-fallback)
  ;; (setq lsp-use-plists t) - disabled as currently getting errors
  )

;;; Clojure language server: clj-pulse or clojure-lsp
;;
;; clj-pulse (https://github.com/abogoyavlensky/clj-pulse) is a native,
;; tree-sitter based server that indexes in stages and uses a fraction of
;; clojure-lsp's memory - which adds up with one server per git worktree.
;; It has no refactorings, code lens or go-to-implementation yet.
;;
;; It is registered as its own lsp-mode client, `clj-pulse', next to
;; lsp-clojure's `clojure-lsp' one, so messages and buffers name the server
;; that actually runs.  Its higher priority wins whenever its activation
;; function agrees, i.e. `my/clojure-language-server' is `clj-pulse' in the
;; buffer and the binary is installed (via mise from ~/dotfiles); otherwise
;; lsp-mode falls back to clojure-lsp.

(defun my/clojure-language-server? (value)
  "Return non-nil if VALUE names a supported Clojure language server."
  (memq value '(clj-pulse clojure-lsp)))

(defcustom my/clojure-language-server 'clj-pulse
  "Language server lsp-mode starts for Clojure buffers.
Set it per project in .dir-locals.el, for example:

  ((nil . ((my/clojure-language-server . clojure-lsp))))

`my/use-clojure-language-server' changes the global value and restarts
the running Clojure workspaces; `my/use-clojure-language-server-in-project'
switches only the current project."
  :type '(choice (const :tag "clj-pulse (native, navigation-focused)" clj-pulse)
                 (const :tag "clojure-lsp (full-featured, JVM analysis)" clojure-lsp))
  :safe #'my/clojure-language-server?
  :group 'lsp-clojure)

(defvar my/clojure-language-server-overrides (make-hash-table :test #'equal)
  "Project root to the Clojure language server picked for it live.
Filled by `my/use-clojure-language-server-in-project'.  An entry wins
over `my/clojure-language-server', dir-local or global, until Emacs exits.")

(defconst my/clj-pulse-major-modes
  '(clojure-mode clojurec-mode clojurescript-mode
    clojure-ts-mode clojure-ts-clojurec-mode clojure-ts-clojurescript-mode
    edn-mode)
  "Major modes clj-pulse serves, the same set as lsp-clojure's client.")

(defun my/clojure-project-root (file-name)
  "Return the lsp session folder that contains FILE-NAME, or nil."
  (and file-name
       (featurep 'lsp-mode)
       (lsp-find-session-folder (lsp-session) file-name)))

(defun my/effective-clojure-language-server (file-name)
  "Return the Clojure language server for FILE-NAME in the current buffer.
A live choice for the file's project wins over `my/clojure-language-server'."
  (or (gethash (my/clojure-project-root file-name) my/clojure-language-server-overrides)
      my/clojure-language-server))

(defun my/clj-pulse-activate? (file-name mode)
  "Return non-nil if clj-pulse should serve FILE-NAME's buffer in MODE.
Reads `my/clojure-language-server' in the buffer, so a dir-local value
picks the server per project, unless the project has a live choice."
  (and (memq mode my/clj-pulse-major-modes)
       (eq (my/effective-clojure-language-server file-name) 'clj-pulse)))

(defun my/clj-pulse-jar-file (uri)
  "Return a local copy of the library source behind the jar URI.
clj-pulse answers go-to-definition into dependencies with
jar:file:///path/lib.jar!/ns/file.clj URIs.  The source is fetched once
via clojure/dependencyContents into a per-jar directory under
`lsp-clojure-workspace-cache-dir', which is a library folder."
  (let ((decoded (url-unhex-string uri)))
    (unless (string-match "\\`jar:file://\\(.+\\)!/\\(.+\\)\\'" decoded)
      (error "Unexpected clj-pulse jar URI: %s" uri))
    (let* ((jar (match-string 1 decoded))
           (entry (match-string 2 decoded))
           (file (expand-file-name entry (file-name-concat lsp-clojure-workspace-cache-dir
                                                           "clj-pulse" (file-name-base jar)))))
      (unless (file-readable-p file)
        (let ((source (with-lsp-workspace (lsp-find-workspace 'clj-pulse nil)
                        (lsp-request "clojure/dependencyContents" (list :uri uri)))))
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert source))))
      file)))

(with-eval-after-load 'lsp-clojure
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("clj-pulse"))
    :activation-fn #'my/clj-pulse-activate?
    :priority 1                         ; beats lsp-clojure's client (0)
    :library-folders-fn (lambda (_workspace) lsp-clojure-library-dirs)
    :uri-handlers (lsp-ht ("jar" #'my/clj-pulse-jar-file))
    ;; Status updates for the VS Code extension; lsp-mode warns on
    ;; notifications nobody handles.
    :notification-handlers (lsp-ht ("clojurePulse/lintStatus" #'ignore)
                                   ("clojurePulse/librariesChanged" #'ignore))
    :server-id 'clj-pulse)))

(defun my/clojure-workspace? (workspace)
  "Return non-nil if WORKSPACE runs one of the Clojure language servers."
  (memq (lsp--client-server-id (lsp--workspace-client workspace))
        '(clj-pulse clojure-lsp)))

(defun my/clojure-workspaces ()
  "Return the running lsp workspaces of either Clojure server."
  (-filter #'my/clojure-workspace? (lsp--session-workspaces (lsp-session))))

(defun my/read-clojure-language-server (current)
  "Read a Clojure language server, defaulting to the one that is not CURRENT."
  (intern (completing-read "Clojure language server: "
                           '("clj-pulse" "clojure-lsp") nil t nil nil
                           (if (eq current 'clj-pulse) "clojure-lsp" "clj-pulse"))))

(defun my/use-clojure-language-server (server)
  "Use SERVER for Clojure buffers and restart the running Clojure workspaces.
SERVER is `clj-pulse' or `clojure-lsp'.  Projects that set
`my/clojure-language-server' in .dir-locals.el, or switched with
`my/use-clojure-language-server-in-project', keep their own choice."
  (interactive (list (my/read-clojure-language-server my/clojure-language-server)))
  (customize-set-variable 'my/clojure-language-server server)
  (when (featurep 'lsp-mode)
    (dolist (workspace (my/clojure-workspaces))
      (lsp-workspace-restart workspace)))
  (message "Clojure buffers now use %s" server))

(defun my/use-clojure-language-server-in-project (server)
  "Restart the current project's Clojure language server as SERVER.
Only this project changes.  The choice wins over .dir-locals.el and the
global `my/clojure-language-server' until Emacs exits; picking the server
the project is configured with drops it again."
  (interactive
   (list (my/read-clojure-language-server
          (my/effective-clojure-language-server buffer-file-name))))
  (let* ((root (or (my/clojure-project-root buffer-file-name)
                   (user-error "Not visiting a file of an lsp project")))
         (running (-filter #'my/clojure-workspace?
                           (gethash root (lsp-session-folder->servers (lsp-session)))))
         (stale (--remove (eq (lsp--client-server-id (lsp--workspace-client it)) server)
                          running)))
    (if (eq server my/clojure-language-server)
        (remhash root my/clojure-language-server-overrides)
      (puthash root server my/clojure-language-server-overrides))
    (cond
     (stale (mapc #'lsp-workspace-restart stale))
     ((and (not running) (memq major-mode my/clj-pulse-major-modes)) (lsp)))
    (message "%s uses %s" (abbreviate-file-name root) server)))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-use-childframe t)    ; Show docs for symbol at point
  )

(use-package lsp-ui-doc
  :ensure nil
  :after lsp-mode
  :custom
  (lsp-ui-doc-position 'at-point)  ; Show doc childframe at point, not top-right
  :bind ((:map lsp-command-map
               ("d" . lsp-ui-doc-glance))))

(use-package lsp-pyright
  :preface
  (defun lsp-pyright-hook ()
    (require 'lsp-pyright)
    (lsp))
  (defun os/locate-python-virtualenv ()
    "Find the Python executable based on the VIRTUAL_ENV environment variable."
    (when-let ((venv (getenv "VIRTUAL_ENV")))
      (let ((python-path (expand-file-name "bin/python" venv)))
        (when (file-executable-p python-path)
          python-path))))
  :vc (:url "https://github.com/emacs-lsp/lsp-pyright")
  :config (add-to-list 'lsp-pyright-python-search-functions #'os/locate-python-virtualenv)
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook ((python-mode . lsp-pyright-hook)
         (python-ts-mode . lsp-pyright-hook)))

(use-package lsp-java
  :ensure t
  :defer t
  :custom
  ;; jdt.ls 1.48 cannot run on the Arch system JDK 26: its OSGi weaving hook
  ;; dies with "Unsupported class file major version 70" (m2e bundle fails,
  ;; server exits with code 13).  Run it on the mise-managed Temurin 25 LTS
  ;; instead; falls back to system `java' if no mise Temurin 25 is installed.
  (lsp-java-java-path
   (or (car (last (file-expand-wildcards
                   (expand-file-name
                    "~/.local/share/mise/installs/java/temurin-25*/bin/java"))))
       "java")))

(defun my/use-lsp-completion-only-as-fallback ()
  "Fallback on lsp completion."
  (when (-contains? completion-at-point-functions #'lsp-completion-at-point)
    (remove-hook 'completion-at-point-functions #'tags-completion-at-point-function t)
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
    (remove-hook 'completion-at-point-functions t t)
    (add-to-list 'completion-at-point-functions #'lsp-completion-at-point t)
    (add-to-list 'completion-at-point-functions t t)))

(defun my/silence-some-lsp-info-messages (orig-fn &rest args)
  "Apply ORIG-FN LSP messages unless ARGS refer to connection actions."
  (unless (or (string-equal (car args) "Connected to %s.")
              (string-equal (car args) "Disconnected"))
    (apply orig-fn args)))

(provide 'setup-lsp)
;;; setup-lsp.el ends here
