;; CIDER
;;
;; Extends Emacs with support for interactive programming in Clojure. The
;; features are centered around cider-mode, an Emacs minor-mode that complements
;; clojure-mode. While clojure-mode supports editing Clojure source files,
;; cider-mode adds support for interacting with a running Clojure process for
;; compilation, debugging, definition and documentation lookup, running tests
;; and so on.

(defun nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(defun my/shadow-cider-keys-with-warning ()
  "Rebind all keys from `cider-mode-map` to `nrepl-warn-when-not-connected` in `clojure-mode-map`."
  (interactive)
  (map-keymap
   (lambda (key def)
     ;; Check if 'def' is a command or another keymap.
     (cond ((commandp def)
            ;; If 'def' is a command, rebind it in `clojure-mode-map`.
            (define-key clojure-mode-map (vector key) 'nrepl-warn-when-not-connected))
           ((keymapp def)
            ;; If 'def' is another keymap, recursively apply the same process.
            (map-keymap
             (lambda (sub-key sub-def)
               (when (commandp sub-def)
                 (define-key clojure-mode-map (vconcat (vector key) (vector sub-key))
                             'nrepl-warn-when-not-connected)))
             def))))
   cider-mode-map))

(use-package cider
  :after (clojure-mode)
  :diminish " CIDER"
  :defer t

  :bind ((:map cider-mode-map
               ([remap cider-quit] . sesman-quit))
         (:map cider-repl-mode-map
               ([remap cider-quit] . sesman-quit)))
  :config
  ;; Warn about missing nREPL instead of doing stupid things
  (my/shadow-cider-keys-with-warning)

  ;; Show the port number when figwheel comes online
  (add-to-list 'cider--repl-stderr-functions #'my/cider-maybe-log-figwheel-main-port)

  ;; Clear CIDER repl buffer with C-c C-l
  (define-key cider-mode-map (kbd "C-c C-l") 'cider-find-and-clear-repl-buffer)
  (define-key cider-repl-mode-map (kbd "C-c C-l") 'cider-repl-clear-buffer)
  (define-key clojure-mode-map (kbd "C-c C-l") 'nrepl-warn-when-not-connected)

  ;; Keybinding to switch to repl-buffer even if it is the wrong kind
  (define-key clojure-mode-map (kbd "C-c z") 'my/cider-select-repl-buffer)
  (define-key cider-repl-mode-map (kbd "C-c z") 'my/cider-select-repl-buffer)

  (autoload 'cider-run-in-dev-namespace "cider-run")
  (define-key clojure-mode-map (kbd "s-:") 'cider-run-in-dev-namespace)
  (define-key cider-repl-mode-map (kbd "s-:") 'cider-run-in-dev-namespace)

  ;; Keybind for reset system
  (define-key clojure-mode-map (kbd "C-:") 'my/cider-run-reset)
  (define-key cider-mode-map (kbd "C-:") 'my/cider-run-reset)

  (define-key cider-mode-map (kbd "C-c M-w") 'my/cider-eval-to-clipboard)
  (define-key cider-mode-map (kbd "C-c C-M-w") 'my/cider-eval-defun-to-clipboard)

  :custom
  ;; save files when evaluating them
  (cider-save-file-on-load t)

  ;; try working around a bug where sesman is way too friendly when selecting
  ;; repl-s from adjacent projects
  ;; https://clojurians.slack.com/archives/C0617A8PQ/p1718608002044069
  (sesman-use-friendly-sessions nil)

  ;; don't pop up repl when connecting
  (cider-repl-pop-to-buffer-on-connect nil)

  ;; re-use dead buffers without asking me about it when there is only one choice
  (cider-reuse-dead-repls 'auto)

  ;; show stacktraces for everything, until https://github.com/clojure-emacs/cider/issues/3495 is solved
  (cider-clojure-compilation-error-phases nil)

  ;; always scroll output from interactive evaluations into view
  (cider-repl-display-output-before-window-boundaries t))

(defun my/cider-maybe-log-figwheel-main-port (buffer out)
  (when (string-match-p "\\[Figwheel\\] Starting Server at" out)
    (message (propertize out 'face 'cider-repl-stderr-face))))

(defun my/get-sessions-with-same-project-dir (session sessions)
  "Returns a list of SESSIONS with the same project-dir as SESSION."
  (--filter
   (s-equals? (plist-get (cider--gather-session-params session) :project-dir)
              (plist-get (cider--gather-session-params it) :project-dir))
   sessions))

(defun my/cider-run-reset ()
  (interactive)
  (with-current-buffer (cider-current-repl)
    (cider-interactive-eval "(reset)")))

(defun my/cider-select-repl-buffer ()
  (interactive)
  (pop-to-buffer
   (let ((buffer-names (-map #'buffer-name
                             (or (cider--extract-connections (my/get-sessions-with-same-project-dir
                                                              (sesman-current-session 'CIDER)
                                                              (sesman-current-sessions 'CIDER)))
                                 (user-error "No linked %s sessions" 'CIDER)))))
     (if (cdr buffer-names)
         (completing-read "Which REPL, Sire?" buffer-names nil t)
       (car buffer-names)))))

(defun cider-find-and-clear-repl-buffer ()
  (interactive)
  (cider-find-and-clear-repl-output t))

(defun my/cider-eval-to-clipboard ()
  "Evaluate the Clojure form at point and put the result on the clipboard."
  (interactive)
  (let ((form (cider-last-sexp)))
    (cider-nrepl-request:eval
     form
     (lambda (response)
       (when (nrepl-dict-get response "value")
         (let ((result (nrepl-dict-get response "value")))
           (kill-new result)
           (message "Result copied to clipboard: %s" result))))
     (cider-current-ns))))

(defun my/cider-eval-defun-to-clipboard ()
  "Evaluate the current top-level form and copy the result to the clipboard."
  (interactive)
  (cider-nrepl-request:eval
   (cider-defun-at-point)
   (lambda (response)
     (when (nrepl-dict-get response "value")
       (let ((result (nrepl-dict-get response "value")))
         (kill-new result)
         (message "Result copied to clipboard: %s" result))))
   (cider-current-ns)))

(provide 'setup-cider)
