;;; setup-functions.el --- Custom functions -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package functions
  :no-require
  :functions (dbus-color-theme-dark-p)
  :init (require 'bind-key)
  :bind (("M-Q" . split-pararagraph-into-lines)
         ("C-M-;" . mark-end-of-sentence)
         ("C-g" . os/keyboard-quit-dwim)
         (:map prog-mode-map
               ("C-<return>" . os/open-line-below)
               ("M-<return>" . os/open-line-above)))
  :preface
  (require 'subr-x)
  (defun os/pfa-invoices-path ()
    "Returns current invoices path"
    (interactive)
    (insert "~/Dropbox/Stoica Ovidiu/2024/chitante"))
  (defconst os/user-name (getenv "USER") "Current user name from environment")
  (defun os/path-with-dynamic-user (path)
    "Build a PATH with the current user's home directory.
     Useful when $HOME doesn't work."
    (expand-file-name (format path os/user-name) "/"))
  (defun os/create-file-in-project-root (filename content)
    (interactive)
    (let ((file-path (expand-file-name filename (project-root (project-current)))))
      (with-temp-file file-path
        (insert content))))
  (defun os-clear-xcode-simulator-cache ()
    (interactive)
    (shell-command "rm -rf ~/Library/Developer/Xcode/DerivedData")
    (shell-command "rm -rf ~/Library/Caches/com.apple.dt.Xcode")
    (shell-command "rm -rf ~/Library/Developer/CoreSimulator/Caches/")
    (message "Simulator cache cleared succesfully!"))

  (defun os/project-shell-command (command)
    "Runs COMMAND in the current project root"
    (let ((default-directory (project-root (project-current))))
      (shell-command command)))

  (defun os/project-file-exists-p (filename)
    "Checks if FILENAME exists in the project root"
    (let ((project-file (expand-file-name filename (project-root (project-current)))))
      (file-exists-p project-file)))

  (defun os/initialize-python-venv ()
    "Initiate python venv and load it with specified Python version"
    (interactive)
    (let* ((python-version (or (completing-read "Python version: " '("python3" "python3.9" "python3.10" "python3.11" "python3.12" "python3.13") nil nil "python3"))))
      (os/project-shell-command (format "%s -m venv .venv" python-version))
      (os/create-file-in-project-root ".envrc"
                                      "export VIRTUAL_ENV=.venv\nlayout python3")
      (envrc-allow)
      (envrc-reload)
      (when (os/project-file-exists-p "requirements.txt")
        (os/project-shell-command "pip install -r requirements.txt"))
      (message "Python venv initialization finished!")))

  (defun os-code-radio ()
    (interactive)
    (browse-url "https://coderadio.freecodecamp.org/"))

  (defun os/open-line-below ()
    "Open a line below the current line."
    (interactive)
    (end-of-line)
    (newline)
    (indent-for-tab-command))

  (defun os/open-line-above ()
    "Open a line above the current line."
    (interactive)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (indent-for-tab-command))

  (defun split-pararagraph-into-lines ()
    "Split the current paragraph into lines with one sentence each."
    (interactive)
    (save-excursion
      (let ((fill-column most-positive-fixnum))
        (fill-paragraph))
      (let ((auto-fill-p auto-fill-function)
            (end (progn (end-of-line) (backward-sentence) (point))))
        (back-to-indentation)
        (unless (= (point) end)
          (auto-fill-mode -1)
          (while (< (point) end)
            (forward-sentence)
            (delete-horizontal-space)
            (newline-and-indent))
          (deactivate-mark)
          (when auto-fill-p
            (auto-fill-mode t))
          (when (looking-at "^$")
            (delete-char -1))))))
  (defun in-termux-p ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))
  (defun dark-mode-enabled-p ()
    "Check if dark mode is enabled."
    (cond ((file-exists-p (expand-file-name "~/.dark-mode")) t)
          ((featurep 'dbus) (dbus-color-theme-dark-p))
          (t nil)))
  (defun memoize (fn)
    "Create a storage for FN's args.
Checks if FN was called with set args before.  If so, return the
value from the storage and don't call FN.  Otherwise calls FN,
and saves its result in the storage.  FN must be referentially
transparent."
    (let ((memo (make-hash-table :test 'equal)))
      (lambda (&rest args)
        ;; `memo' is used as a singleton to check for absense of value
        (let ((value (gethash args memo memo)))
          (if (eq value memo)
              (puthash args (apply fn args) memo)
            value)))))
  (defmacro defmemo (name &rest funtail)
    (declare (doc-string 3) (indent 2) (debug defun))
    `(defalias ',name (memoize (lambda ,@funtail))))
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defmemo\\)\\_>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))))
  (defvar-local ssh-tunnel-port nil)
  (put 'ssh-tunnel-port 'safe-local-variable #'numberp)
  (defun ssh-tunnel (host port &optional local-port)
    "Start an SSH tunnel from localhost to HOST:PORT.
If LOCAL-PORT is nil, PORT is used as local port."
    (interactive (list (read-string "host: " nil 'ssh-host-history)
                       (read-number "port: " ssh-tunnel-port 'ssh-port-history)
                       (when current-prefix-arg
                         (read-number "local port: " ssh-tunnel-port 'ssh-port-history))))
    (let ((name (if (and local-port (not (= local-port port)))
                    (format "*ssh-tunnel:%s:%s:%s" local-port host port)
                  (format "*ssh-tunnel:%s:%s" host port))))
      (async-shell-command
       (format "ssh -4 -N -L %s:localhost:%s %s" (or local-port port) port host)
       (concat " " name))))
  (defun os/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open. Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:
- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p) (keyboard-quit))
     ((derived-mode-p 'completion-list-mode) (delete-completion-window))
     ((> (minibuffer-depth) 0) (abort-recursive-edit))
     (t (keyboard-quit))))
  (provide 'functions)
  )

(provide 'setup-functions)
;;; setup-functions.el ends here
