;; -*- lexical-binding: t -*-
;; #+options: prop:t
;;; init.el --- My personal emacs configuration.
;;; Author: Ovidiu Stoica
;;; Commentary:
;;; The entire configuration is kept here.
;;; Currently written for Emacs 29.
;;; Code:

;; * PACKAGE MANAGEMENT

(defmacro comment (&rest body)
  "Ignore forms in BODY, returning nil. Used for rich comments."
  nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; DEBUG
;; (setq debug-on-error t)
;; (setq debug-on-quit t)


;; ** ELPA
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; ** USE-PACKAGE
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

;; Don't show byte compilation warnings when installing packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(require 'use-package)

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(defvar os/emacs-tmp-dir (concat user-emacs-directory "tmp/")
  "Scratch space for stuff...")

(setq package-user-dir (concat user-emacs-directory "elpa"))
(setq package-native-compile t)

;; ** STRAIGHT
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; * PRIVATE CONFIG

(defvar os/private-config-file (concat user-emacs-directory "emacs-private.el")
  "File with configuration info that can't be in public repository.")
(if (file-readable-p os/private-config-file)
    (progn
      (load-library os/private-config-file)
      (message "Loaded private config")))

;; * BUILTIN


(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (locate-user-emacs-file "early-init.el")))

(use-package delight
  :ensure t)

(use-package compile
  :preface
  (defun os/compile-autoclose (buffer string)
    "Hide successful builds window with BUFFER and STRING."
    (if (string-match "finished" string)
        (progn
          (message "Build finished: ")
          (run-with-timer 3 nil
                          (lambda ()
                            (when-let* ((multi-window (> (count-windows) 1))
                                        (live (buffer-live-p buffer))
                                        (window (get-buffer-window buffer t)))
                              (delete-window window))))
          (message "Compilation %s" string))))
  :config
  (setq compilation-scroll-output t)
  (setq compilation-auto-jump-to-first-error t
        compilation-max-output-line-length nil
        compilation-finish-functions (list #'os/compile-autoclose)))



(use-package emacs
  :config
  (ffap-bindings)
  (push (expand-file-name "plugins/" user-emacs-directory) load-path)
  (push (expand-file-name "lisp/" user-emacs-directory)  load-path))

;; ** MAC STUFF
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))



(use-package local-config
  :no-require
  :preface
  (defgroup local-config ()
    "Customization group for local settings."
    :prefix "local-config-"
    :group 'emacs)
  (defcustom local-config-dark-theme 'modus-vivendi
    "Dark theme to use."
    :tag "Dark theme"
    :type 'symbol
    :group 'local-config)
  (defcustom local-config-light-theme 'modus-operandi
    "Light theme to use."
    :tag "Light theme"
    :type 'symbol
    :group 'local-config)
  (defcustom no-hscroll-modes '(term-mode)
    "Major modes to disable horizontal scrolling."
    :tag "Modes to disable horizontal scrolling"
    :type '(repeat symbol)
    :group 'local-config)
  (provide 'local-config))

;; ** FUNCTIONS


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
  (provide 'functions))

;; ** DEFAULTS

(use-package defaults
  :no-require
  :preface
  (setq-default
   indent-tabs-mode nil
   load-prefer-newer t
   truncate-lines t
   bidi-paragraph-direction 'left-to-right
   frame-title-format
   '((:eval
      (if (buffer-file-name)
          (abbreviate-file-name (buffer-file-name))
        (buffer-name))))
   auto-window-vscroll nil
   mouse-highlight t
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   frame-resize-pixelwise window-system
   window-resize-pixelwise window-system
   fill-column 80)
  (when (window-system)
    (setq-default
     x-gtk-use-system-tooltips nil
     cursor-type 'box
     blink-cursor-mode 0
     cursor-in-non-selected-windows nil))
  (setq
   ring-bell-function 'ignore
   mode-line-percent-position nil
   enable-recursive-minibuffers t)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (global-visual-line-mode t)
  (custom-safe-themes
   '("712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" default))
  (custom-set-variables
   '(safe-local-variable-values
     '((eval progn
             (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
             (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
       (cider-clojure-cli-aliases . "-A:dev:test:reveal")
       (eval progn
             (setq cider-clojure-cli-aliases ":dev:test")
             (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
             (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware")
             (setq cider-format-code-options
                   '(("indents"
                      (("$"
                        (("block" 1))))))))
       (cider-default-cljs-repl . custom)
       (cider-clojure-cli-aliases . ":dev:test")
       (cider-preferred-build-tool . clojure-cli))))
  (provide 'defaults))

;; * WINDOW MANAGEMENT
;; ** WINDMOVE
(use-package windmove
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings ))


;; ** POPPER
(defvar os/occur-grep-modes-list '(occur-mode
                                   grep-mode
                                   xref--xref-buffer-mode
                                   ivy-occur-grep-mode
                                   ivy-occur-mode
                                   locate-mode
                                   flymake-diagnostics-buffer-mode
                                   rg-mode)
  "List of major-modes used in occur-type buffers.")

;; This does not work at buffer creation since the major-mode for
;; REPLs is not yet set when `display-buffer' is called, but is
;; useful afterwards
(defvar os/repl-modes-list '(matlab-shell-mode
                             eshell-mode
                             geiser-repl-mode
                             shell-mode
                             eat-mode
                             vterm-mode
                             inferior-python-mode
                             cider-repl-mode
                             fennel-repl-mode
                             jupyter-repl-mode
                             inferior-ess-julia-mode)
  "List of major-modes used in REPL buffers.")

(defvar os/repl-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*MATLAB\\*"
    "\\*Python\\*"
    "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
    "\\*Inferior .*\\*$"
    "^\\*julia.*\\*$"
    "\\*cider-repl.*\\*$"
    "\\*ielm\\*"
    "\\*nodejs\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

(defvar os/help-modes-list '(helpful-mode
                             help-mode
                             pydoc-mode
                             TeX-special-mode)
  "List of major-modes used in documentation buffers.")

(defvar os/man-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar os/message-modes-list '(compilation-mode
                                edebug-eval-mode)
  "List of major-modes used in message buffers.")

(use-package popper
  :straight '(:type git :host github :repo "karthink/popper")
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        (append os/help-modes-list
                os/man-modes-list
                os/repl-modes-list
                os/repl-names-list
                os/occur-grep-modes-list
                ;; my/man-modes-list
                '(Custom-mode
                  compilation-mode
                  messages-buffer-mode)
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                  "^\\*Matlab Help.*\\*$"
                  ;; "^\\*Messages\\*$"
                  "^\\*Backtrace\\*"
                  "^\\*evil-registers\\*"
                  "^\\*Apropos"
                  "^Calc:"
                  "^\\*eldoc\\*"
                  "^\\*TeX errors\\*"
                  "^\\*ielm\\*"
                  "^\\*TeX Help\\*"
                  "^\\*ChatGPT\\*"
                  "^\\*Input History\\*"
                  "^\\*gptel-ask\\*"
                  "^\\*clojure-compilation\\*"
                  "\\*Shell Command Output\\*"
                  "\\*Async Shell Command\\*"
                  "\\*[cC]ausal-[^*]+\\*"
                  ("\\*Detached Shell Command\\*" . hide)
                  "\\*Completions\\*"
                  ;; "\\*scratch.*\\*$"
                  "[Oo]utput\\*")))
  (add-to-list 'display-buffer-alist '("\\*ielm\\*"
                                       (display-buffer-in-side-window)
                                       (side . left)
                                       (window-width . 50)))
  (add-to-list 'display-buffer-alist '("\\*cider-repl.*\\*$"
                                       (display-buffer-in-side-window)
                                       (side . left)
                                       (window-width . 50)))
  (add-to-list 'display-buffer-alist
               '("^\\*cider-repl .+\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 70)))
  (add-to-list 'display-buffer-alist
               '("^\\*cider-repl .*\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 120)))
  (popper-mode +1)
  (popper-echo-mode +1))

;; ** CORE PACKAGES

(use-package delsel
  :ensure nil ;; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'string))

(use-package bind-key
  :ensure t)
(require 'bind-key)

(use-package window
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*Calendar*" (display-buffer-at-bottom))))


(use-package mouse
  :bind (("<mode-line> <mouse-2>" . nil)
         ("<mode-line> <mouse-3>" . nil)))

(use-package mode-line
  :no-require
  :preface
  (defvar mode-line-interactive-position
    `(line-number-mode
      (:propertize " %l:%C"
                   help-echo "mouse-1: Goto line"
                   mouse-face mode-line-highlight
                   local-map ,(let ((map (make-sparse-keymap)))
                                (define-key map [mode-line down-mouse-1] 'goto-line)
                                map)))
    "Mode line position with goto line binding.")
  (put 'mode-line-interactive-position 'risky-local-variable t)
  (setq-default mode-line-format ; Sets the default mode-line format for all buffers
                '(               ; List of elements to display in the mode-line
                  "%e"         ; Shows error message if there's not enough space
                  mode-line-front-space ; Space at beginning

                  mode-line-modified ; Shows if buffer is modified (*) or read-only (%)
                  mode-line-remote   ; Shows @ if file is remote
                  mode-line-frame-identification ; Shows - if windowed, or terminal name if in terminal

                                        ; Custom project display section
                  (:propertize ; Adds face properties to the following expression
                   (:eval ; Evaluates the expression each time mode-line updates
                    (when-let ((project (project-current))) ; If in a project
                      (format "[%s] " (project-name project)))) ; Shows [project-name]
                   face warning)        ; Displays in warning face color

                                        ; Buffer name section
                  (:propertize "%b" face mode-line-buffer-id) ; Current buffer name with special face
                  " "                                         ; Space
                  mode-line-position ; Shows position in buffer (line number, %)
                  (vc-mode vc-mode)  ; Version control information
                  " "                ; Space
                  mode-line-modes    ; Major and minor modes
                  mode-line-misc-info    ; Misc information
                  mode-line-end-spaces)) ; Spaces at end

  (provide 'mode-line))

(use-package font
  :no-require
  :hook (after-init . setup-fonts)
  :preface
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  (defun setup-fonts ()
    (cond ((font-installed-p "JetBrainsMono Nerd Font Mono")
           (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono"))
          ((font-installed-p "Source Code Pro")
           (set-face-attribute 'default nil :font "Source Code Pro")))
    (when (font-installed-p "DejaVu Sans")
      (set-face-attribute 'variable-pitch nil :font "DejaVu Sans")))
  (provide 'font))

(use-package cus-edit
  :custom
  (custom-file (locate-user-emacs-file "custom.el"))
  :init
  (load custom-file :noerror))

(use-package novice
  :preface
  (defvar disabled-commands (locate-user-emacs-file "disabled.el")
    "File to store disabled commands, that were enabled permanently.")
  (define-advice enable-command (:around (fn command) use-disabled-file)
    (let ((user-init-file disabled-commands))
      (funcall fn command)))
  :init
  (load disabled-commands 'noerror))

(use-package files
  :preface
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups.")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,backup-dir)))
  (auto-save-file-name-transforms
   `((".*" ,auto-save-dir t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  (require-final-newline t)
  :bind ("<f5>" . revert-buffer-quick)
  :init
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package subr
  :no-require
  :init
  (if (boundp 'use-short-answers)
      (setq-default use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p)))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))


(use-package simple
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-current-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . overwrite-mode-set-cursor-shape)
         (after-init . column-number-mode)
         (prog-mode . display-line-numbers-mode)
         (after-init . line-number-mode))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  (copy-region-blink-delay 0)
  (shell-command-default-error-buffer "*Shell Command Errors*")
  :config
  (defun overwrite-mode-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
  :preface
  (unless (fboundp 'minibuffer-keyboard-quit)
    (autoload #'minibuffer-keyboard-quit "delsel" nil t))
  (define-advice keyboard-quit
      (:around (quit) quit-current-context)
    "Quit the current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
    (if (active-minibuffer-window)
        (if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (unless (or defining-kbd-macro
                  executing-kbd-macro)
        (funcall-interactively quit)))))

(use-package minibuffer
  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))





(use-package diminish
  :ensure t
  :config
  (diminish 'whole-line-or-region-local-mode)
  (diminish 'visual-line-mode))

(use-package bindings
  :bind ( :map ctl-x-map
          ("DEL" . nil)
          ("C-d" . dired-jump))
  :init
  (setq mode-line-end-spaces nil))

(use-package frame
  :requires seq
  :bind (("C-z" . ignore)
         ("C-x C-z" . ignore)))

(use-package startup
  :no-require
  :custom
  (inhibit-splash-screen t))

(use-package menu-bar
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1))

(use-package tooltip
  :when (window-system)
  :custom
  (tooltip-x-offset 0)
  (tooltip-y-offset (line-pixel-height))
  (tooltip-frame-parameters
   `((name . "tooltip")
     (internal-border-width . 2)
     (border-width . 1)
     (no-special-glyphs . t))))

(use-package uniquify
  :defer t
  :custom
  (uniquify-buffer-name-style 'forward))


(use-package display-line-numbers
  :hook (display-line-numbers-mode . toggle-hl-line)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (defun toggle-hl-line ()
    (hl-line-mode (if display-line-numbers-mode 1 -1))))

(use-package pixel-scroll
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package vc-hooks
  :defer t
  :custom
  (vc-follow-symlinks t))

(use-package eldoc
  :delight eldoc-mode
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))





(use-package dired
  :preface
  (defun dired-back-to-top ()
    "Jump to the top file in a Dired buffer."
    (interactive)
    (goto-char (point-min))
    ;; because the number of header lines varies depending on whether
    ;; mode info is shown or hidden, find the double-dot directory entry
    ;; and go forward one line -- heuristic, but will always work.
    (search-forward "..")
    (dired-next-line 1))

  (defun dired-jump-to-bottom ()
    "Jump to the last file in a Dired buffer."
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  (defun dired-home-directory ()
    (interactive)
    (dired (expand-file-name "~/")))
  :commands
  dired
  dired-jump
  dired-next-line
  dired-up-directory
  :bind (:map dired-mode-map
              ("-"                         . dired-up-directory)
              ("~" . dired-home-directory)
              ("E"                         . wdired-change-to-wdired-mode)
              ([remap beginning-of-buffer] . dired-back-to-top)
              ([remap end-of-buffer]       . dired-jump-to-bottom))
  :config
  (setq insert-directory-program "gls"
        dired-use-ls-dired t
        dired-listing-switches "-lAXGh --group-directories-first --sort=extension" ;; directories first
        dired-hide-details-mode t
        dired-recursive-copies 'always
        dired-recursive-copies 'always
        delete-by-moving-to-trash t
        dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)
              ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;; Dired extra font locking
(use-package diredfl
  :ensure t
  :after (dired)
  :hook (dired-mode .diredfl-mode))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package json-hs-extra
  :after json
  :hook (json-ts-mode . json-hs-extra-setup)
  :preface
  (defun json-hs-extra-create-overlays (overlay)
    "Creates overlays for block beginning, hiding whitespace.
Sets OVERLAY `json-hs-extra-overlays' property to the list of created
overlays."
    (let ((end (point)))
      (save-excursion
        (forward-sexp -1)
        (when-let ((overlays (ov-regexp "{[[:space:]\n]*" (point) end)))
          (mapc (lambda (ov) (overlay-put ov 'display "{")) overlays)
          (overlay-put overlay 'json-hs-extra-overlays overlays)))))
  (defun json-hs-extra-delete-overlays (fn overlay)
    "Deletes overlays for block beginning created earlier.
Deletes overlays in the `json-hs-extra-overlays' property of OVERLAY,
created with `json-hs-extra-create-overlays'."
    (mapc #'delete-overlay (overlay-get overlay 'json-hs-extra-overlays))
    (funcall fn overlay))
  (defun json-hs-extra-setup ()
    "Special settings for JSON buffers."
    (setq-local hs-block-start-regexp "\\(?:{[[:space:]\n]*\\|\\[\\)"
                hs-set-up-overlay #'json-hs-extra-create-overlays))
  (provide 'json-hs-extra)
  :config
  (advice-add 'delete-overlay :around #'json-hs-extra-delete-overlays))

(use-package help
  :custom
  (help-window-select t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package outline
  :hook (common-lisp-modes-mode . lisp-outline-minor-mode)
  :delight outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))

(use-package browse-url
  :when (fboundp 'xwidget-webkit-browse-url)
  :custom (browse-url-browser-function #'xwidget-webkit-browse-url))

(use-package centered-window :ensure t)


(use-package repeat
  :hook (after-init . repeat-mode))

;; TODO Document on this further, possibly disable LSP on very long files
(use-package so-long
  :init (global-so-long-mode 1))


;; * COMPLETION

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex)))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)                    ; Number of candidates to display
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)?
  (vertico-resize t)
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))


(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map
              ("<tab>" . corfu-complete)
              ([remap completion-at-point] . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0
        corfu-cycle t
        corfu-on-exact-match nil) ;; Don't auto insert

  (corfu-popupinfo-mode 1)   ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))



(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config (setq corfu-popupinfo-delay '(0.5 . 0.2))
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :commands (corfu-terminal-mode)
  :hook (after-init . corfu-terminal-mode))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions '(cape-file)))

(use-package ov
  :ensure t
  :commands (ov-regexp))

(use-package abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-package consult
  :ensure t
  :defines
  consult-customize
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)    ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-lsp
  :after consult
  :straight '(consult-lsp :type git :host github :repo "gagbo/consult-lsp"))

(use-package which-key
  :ensure which-key
  :diminish
  :functions
  which-key-mode
  :config
  (which-key-mode))

;; * FORMATTING

;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure apheleia
  :diminish ""                          ; Don't show in modeline
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i=4" "-sr" "-kp"))
  (setq apheleia-log-debug-info t)
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'standard-clojure apheleia-formatters)
        '("standard-clj" "fix" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurec-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurescript-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-json)
  (setf (alist-get 'prettier-css apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier-css)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css)

  (apheleia-global-mode +1))

;; * NERD ICONS

(use-package nerd-icons
  :straight '(nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))

(use-package nerd-icons-dired
  :straight '(nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :straight '(nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :straight '(nerd-icons-corfu :type git :host github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))



;; * ORG

(use-package org
  :preface


  :hook ((org-babel-after-execute . org-redisplay-inline-images)
         (org-mode . org-indent-mode))
  :bind (("C-c A" . org-agenda)
         ("C-c C-l" . org-store-link)
         :map org-mode-map
         ("C-c l" . org-store-link))
  :custom
  ;; Your existing custom settings remain unchanged
  (org-M-RET-may-split-line '((default . nil)))
  (org-support-shift-select t)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-image-actual-width nil)
  (org-src-preserve-indentation t)
  (org-agenda-files (list
                     "~/org/todo_agenda.org"
                     "~/workspace/voice-fn/TODO.org"
                     "~/Dropbox/todo/todo.org"))
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (setq org-special-ctrl-a/e t)
  (setq org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-agenda-tags-column 0)
  ;; Ellipsis styling
  (setq org-ellipsis "...")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo)))

(use-package org-modern
  :disabled t
  :init (global-org-modern-mode)
  :config
  (setq org-modern-star 'replace)
  (setq org-modern-timestamp nil))

(use-package org-pomodoro
  :ensure t)

(use-package ob-shell :after org)

(use-package org-capture
  :bind ( :map mode-specific-map
          ("o c" . org-capture)))


(use-package ol
  :after org-capture
  :functions (org-link-set-parameters)
  :preface
  (defun blog-follow-html-link (path arg)
    (funcall browse-url-browser-function path arg))
  (defun blog-export-hmtl-link (path description _backend _properties)
    "Export link directly to HTML."
    (format "<a href=\"%s\">%s</a>" path (or description path)))
  (defun blog-create-html-link (&optional _)
    "Create a file link using completion."
    (let ((link (read-string "Link: ")))
      (concat "blog-html:" link)))
  :config
  (org-link-set-parameters
   "org"
   :export #'blog-export-static-org-link
   :complete #'blog-create-static-org-link)
  (org-link-set-parameters
   "blog-html"
   :follow #'blog-follow-html-link
   :export #'blog-export-hmtl-link
   :complete #'blog-create-html-link))

(use-package ox-hugo
  :ensure t
  :after ox
  :preface
  (declare-function org-export-data "ext:ox")
  (declare-function org-export-get-node-property "ext:ox")
  (define-advice org-hugo-heading (:around (fn heading contents info) patch)
    (if (and (org-export-get-node-property :BLOG-COLLAPSABLE heading) (not (string-empty-p contents)))
        (let ((title (org-export-data (org-element-property :title heading) info)))
          (concat "<details class=\"foldlist\"><summary>" title
                  "</summary><div class=\"foldlistdata\">\n\n"
                  contents
                  "</div></details>"))
      (funcall fn heading contents info))))

(use-package ox-latex
  :after ox)

(use-package epresent
  :ensure t
  :custom
  (epresent-text-scale 200)
  (epresent-format-latex-scale 2)
  :hook
  (epresent-start-presentation . epresent-setup)
  :preface
  (defun epresent-setup ()
    (interactive)
    (visual-line-mode 1)
    (flyspell-mode -1)
    (set-window-fringes (selected-window) 600 600)
    (set-face-attribute
     'org-block (selected-frame)
     :background (modus-themes-get-color-value 'bg-dim))
    (set-face-attribute
     'header-line (selected-frame)
     :height 1200
     :background 'unspecified)
    (setq-local header-line-format " ")))

;; * WRITING

(use-package writegood-mode
  :ensure t
  :hook ((markdown-mode nroff-mode org-mode
                        mail-mode
                        git-commit-mode)
         . writegood-mode))

;; * LANGUAGES

(use-package common-lisp-modes
  :straight '(common-lisp-modes :type git :host gitlab :repo  "andreyorst/common-lisp-modes.el"))

(use-package python-ts-mode
  :preface (defun os/setup-python-environment ()
             "Custom configuration for Python mode."
             (yas-minor-mode 1)
             ;; Set the `python-shell-interpreter' to the python in PATH.
             ;; At this moment `envrc' should succesfully configure environment.
             (setq-local python-shell-interpreter (executable-find "python")))
  :hook (python-ts-mode . os/setup-python-environment))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ( :map markdown-mode-map
          ("M-Q" . split-pararagraph-into-lines))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-hr-display-char nil)
  (markdown-list-item-bullets '("-")))

(use-package elisp-mode
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

(use-package csv-mode
  :ensure t
  :hook ((csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width most-positive-fixnum))

(use-package cc-mode
  :hook (c-mode-common . cc-mode-setup)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun cc-mode-setup ()
    (c-set-offset 'case-label '+)
    (setq-local comment-start "//"
                comment-end ""
                tab-width 4)))

(use-package yaml-mode
  :ensure t
  :defer t
  :custom
  (yaml-indent-offset 4))

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)


(use-package csv-mode
  :ensure t
  :hook ((csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width most-positive-fixnum))


;; ** CLOJURE
(load (expand-file-name "plugins/clj-functions.el" user-emacs-directory))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :commands (clojure-project-dir)
  :bind ( :map clojure-mode-map
          ("C-:" . clj-functions-hiccup-classes-to-string))
  :config
  (defun clojure-set-compile-command ()
    (let ((project-dir (clojure-project-dir)))
      (cond ((and (file-exists-p (expand-file-name "project.clj" project-dir))
                  (executable-find "lein"))
             (setq-local compile-command "lein "))
            ((and (file-exists-p (expand-file-name "deps.edn" project-dir))
                  (executable-find "clojure"))
             (setq-local compile-command "clojure ")))))
  (defun clojure-mode-setup ()
    "Setup Clojure buffer."
    (common-lisp-modes-mode 1)
    (clojure-set-compile-command)))

(use-package cider
  :ensure t
  :after clojure-mode
  :defines
  cider-current-repl
  cider-interractive-eval
  :preface
  (defun cider-reset ()
    (interactive)
    (with-current-buffer (cider-current-repl)
      (cider-interactive-eval "(reset)")))
  :delight " CIDER"
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer)
          ("M-:" . cider-reset)
          :map cider-mode-map
          ("M-¬" . cider-format-buffer)
          ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
          ("M-:" . cider-reset)
          ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))

  :config
  (setq cider-show-eval-spinner t
        cider-allow-jack-in-without-project t
        cider-preferred-build-tool 'clojure-cli
        ;; ~make sure we can always debug nrepl issues~
        ;; Turning this off again, seems it may really blow up memory usage
        nrepl-log-messages nil)
  (setq cider-download-java-sources t))

(use-package ob-clojure
  :after (org clojure-mode)
  :custom
  (org-babel-clojure-backend 'cider)
  :init
  (require 'cider))

(use-package clj-refactor
  :ensure t
  :delight clj-refactor-mode
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))

(use-package clj-decompiler
  :ensure t
  :hook (cider-mode . clj-decompiler-setup))

(use-package babashka
  :ensure t)

(use-package lisp-mode
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

(use-package sql-indent
  :defer t
  :ensure t)

(use-package sqlite-mode-extras
  :straight '(sqlite-mode-extras :type git :host github :repo "xenodium/sqlite-mode-extras")
  :defines sqlite-mode-map
  :hook ((sqlite-mode . sqlite-extras-minor-mode))
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))


(use-package groovy-mode
  :straight '(groovy-mode :type git :host github :repo "Groovy-Emacs-Modes/groovy-emacs-modes")
  :mode "\\.gradle\\'"                ; if you want this mode to be auto-enabled
  )


(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :diminish ""
  :straight '(highlight-indent-guides :type git :host github :repo "DarthFennec/highlight-indent-guides")
  :defines
  highlight-indent-guides-method
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))


(use-package hl-todo
  :straight '(hl-todo :type git :host github :repo "tarsius/hl-todo")
  :init (global-hl-todo-mode))

(use-package envrc
  :straight '(envrc :type git :host github :repo "purcell/envrc")
  :init (envrc-global-mode))

;; * TREE-SITTER MODES
(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.py\\'" . python-ts-mode)
         ("\\.cmake\\'" . cmake-ts-mode)
         ("\\.java\\'" . java-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.php\\'" . php-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mm\\'" . objc-mode)
         ("\\.mdx\\'" . markdown-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java"))
               (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (objc "https://github.com/tree-sitter-grammars/tree-sitter-objc")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (php "https://github.com/tree-sitter/tree-sitter-php" "v0.22.8" "php/src" )
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package combobulate
  :after (treesit)
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  :bind (:map combobulate-key-map
              ("M-a" . nil)
              ("M-n" . nil)
              ("M-p" . nil))
  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook
  ((python-ts-mode . combobulate-mode)
   (java-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (go-mode . go-ts-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("~/workspace/combobulate"))

;; * LSP

(use-package lsp-mode
  :ensure t
  :diminish "LSP"
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (defun os/lsp-gopls-hook ()
    (lsp-register-custom-settings '(("gopls.completeUnimported" t t)
                                    ("gopls.staticcheck" t t))))

  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           json-ts-mode
           js-ts-mode
           java-ts-mode
           go-ts-mode
           python-ts-mode
           prisma-ts-mode
           clojure-mode
           clojurec-mode
           clojurescript-mode
           go-ts-mode) . lsp)
         (go-ts-mode . os/lsp-gopls-hook))
  :config

  (define-key lsp-command-map (kbd "d") #'lsp-ui-doc-glance)
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("/opt/homebrew/bin/clojure-lsp"))


  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none) ;; we use Corfu
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)   ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)    ; Use xref to find references
  (lsp-auto-configure t) ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t) ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil) ; This basically means, if you change
                                        ; the branch, should LSP reload
                                        ; instantly - it's a big performance
                                        ; overhead
  (lsp-enable-folding nil)     ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)    ; I use prettier
  (lsp-enable-links nil)          ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)  ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil) ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)    ; Important to provide full JSX completion
  (lsp-completion-show-kind t)         ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t) ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil) ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1) ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)        ; Show docs for symbol at point
  (lsp-eldoc-render-all nil) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :bind
  (:map lsp-mode-map
        ([remap lsp-treemacs-errors-list] . consult-lsp-diagnostics)
        ([remap consult-imenu] . consult-lsp-file-symbols)
        ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode-maybe))
  :commands (lsp-completion-mode)
  :preface
  (defun lsp-completion-mode-maybe ()
    (unless (bound-and-true-p cider-mode)
      (lsp-completion-mode 1))))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :after (lsp-mode)
  :config (setq lsp-ui-doc-enable t
                ;; evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point)) ; Display the doc under the point

(use-package lsp-treemacs
  :ensure t
  :defer t
  :custom
  (lsp-treemacs-theme "Iconless"))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq lsp-tailwindcss-major-modes '(clojure-mode tsx-ts-mode css-ts-mode clojurescript-mode clojurec-mode)
        lsp-tailwindcss-experimental-class-regex
        [":class\\s+\"([^\"]*)\""
         ":[\\w-.#>]+\\.([\\w-]*)"
         "tw|yourModule\\(([^)]*)\\)"
         "[\"'`]([^\"'`]*).*?[\"'`]"
         ]
        lsp-tailwindcss-class-attributes ["class" "className" "ngClass" ":class"]))


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
  :straight '(lsp-pyright :type git :host github :repo "emacs-lsp/lsp-pyright")
  :config (add-to-list 'lsp-pyright-python-search-functions #'os/locate-python-virtualenv)
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook ((python-mode . lsp-pyright-hook)
         (python-ts-mode . lsp-pyright-hook)))

(use-package lsp-eslint
  :demand t
  :after lsp-mode
  :config
  ;; Use latest LSP from VSCode installed
  (setq lsp-eslint-server-command (list "node"
                                        (os/path-with-dynamic-user "/Users/%s/.vscode/extensions/dbaeumer.vscode-eslint-3.0.10/server/out/eslintServer.js")
                                        "--stdio")))

(use-package lsp-java
  :ensure t)

;; * Navigation & Editing

(use-package easy-kill
  :disabled t
  :ensure t
  :bind (([remap mark-sexp] .'mark-sexp)))

(use-package whole-line-or-region
  :ensure t
  :diminish ""
  :init
  (whole-line-or-region-global-mode 1))

(use-package common-lisp-modes
  :delight common-lisp-modes-mode
  :preface
  (defun indent-sexp-or-fill ()
    "Indent an s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . indent-sexp-or-fill)))

(use-package region-bindings
  :straight '(region-bindings :type git :host gitlab :repo "andreyorst/region-bindings.el")
  :commands (region-bindings-mode)
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook ((after-init . global-region-bindings-mode)
         ((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off)))

(use-package puni
  :ensure t
  :hook (((common-lisp-modes-mode nxml-mode json-ts-mode prog-mode org-mode) . puni-mode)
         (puni-mode . electric-pair-local-mode))
  :bind ( :map region-bindings-mode-map
          ("(" . puni-wrap-round)
          ("[" . puni-wrap-square)
          ("{" . puni-wrap-curly)
          ("<" . puni-wrap-angle)
          ;; paredit-like keys
          :map puni-mode-map
          ("C-M-f" . puni-forward-sexp-or-up-list)
          ("C-M-b" . puni-backward-sexp-or-up-list)
          ("C-M-t" . puni-transpose)
          ;; slurping & barfing
          ("C-<left>" . puni-barf-forward)
          ("C-}" . puni-barf-forward)
          ("C-<right>" . puni-slurp-forward)
          ("C-)" . puni-slurp-forward)
          ("C-(" . puni-slurp-backward)
          ("C-M-<left>" . puni-slurp-backward)
          ("C-{" . puni-barf-backward)
          ("C-M-<right>" . puni-barf-backward)
          ;; depth chaning
          ("M-r" . puni-raise)
          ("M-<up>" . puni-splice-killing-backward)
          ("M-<down>" . puni-splice-killing-forward)
          ("M-(" . puni-wrap-round)
          ("M-{" . puni-wrap-curly)
          ("M-[" . puni-wrap-square)
          ("M-?" . puni-convolute)
          ("M-R" . puni-splice)
          ("M-S" . puni-split)))

(use-package isearch
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char))
  :custom
  (isearch-lazy-highlight t))

(use-package phi-search
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :bind
  ( :map avy-map
    ("M-w l" . avy-kill-ring-save-whole-line)
    ("M-k l" . avy-kill-whole-line)
    ("M-w r" . avy-kill-ring-save-region)
    ("M-k r" . avy-kill-region)
    ("c" . avy-goto-char-timer)
    ("l" . avy-goto-line)
    ("n" . avy-next)
    ("p" . avy-prev))
  :preface
  (defalias 'avy-map-prefix (make-sparse-keymap))
  (defvar avy-map (symbol-function 'avy-map-prefix)
    "Keymap for characters following \\`M-a'.")
  (define-key global-map (kbd "M-a") 'avy-map-prefix))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(bind-key "M-o"         #'other-window)
(bind-key "M-i"         #'consult-imenu)

(use-package isayt
  :straight '(isayt :type git :host gitlab :repo "andreyorst/isayt.el")
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package multiple-cursors
  :ensure t
  :bind
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   :map region-bindings-mode-map
   ("n" . mc/mark-next-symbol-like-this)
   ("N" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-symbol-like-this)
   ("P" . mc/mark-previous-like-this)
   ("a" . mc/mark-all-symbols-like-this)
   ("A" . mc/mark-all-like-this)
   ("s" . mc/mark-all-in-region-regexp)
   ("l" . mc/edit-ends-of-lines)))

(use-package multiple-cursors-core
  :bind
  (( :map mc/keymap
     ("<return>" . nil)
     ("C-&" . mc/vertical-align-with-space)
     ("C-#" . mc/insert-numbers))))

(use-package separedit
  :ensure t
  :hook (separedit-buffer-creation . separedit-header-line-setup)
  :bind ( ("C-c '" . separedit)
          :map separedit-mode-map
          ("C-c C-c" . separedit-commit)
          :map edit-indirect-mode-map
          ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  :config
  (nconc (assoc '(";+") separedit-comment-delimiter-alist)
         '(clojure-mode clojurec-mode clojure-script-mode))
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with \\<edit-indirect-mode-map>`\\[edit-indirect-abort]'"))))

(use-package vundo
  :ensure t
  :bind ( :map mode-specific-map
          ("u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :init (yas-global-mode 1))

;; * PROJECT

(use-package project
  :ensure t
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-vc-extra-root-markers
   '("Cargo.toml" "compile_commands.json" "pyproject.toml"
     "compile_flags.txt" "project.clj"
     "deps.edn" "shadow-cljs.edn" "package.json"))
  :preface
  (defcustom project-compilation-mode nil
    "Mode to run the `compile' command with."
    :type 'symbol
    :group 'project
    :safe #'symbolp
    :local t)
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))
  (defvar project-compilation-modes nil
    "List of functions to check for specific compilation mode.

The function must return a symbol of an applicable compilation
mode.")
  (define-advice compilation-start
      (:filter-args (args) use-project-compilation-mode)
    (let ((cmd (car args))
          (mode (cadr args))
          (rest (cddr args)))
      (catch 'args
        (when (null mode)
          (dolist (comp-mode-p project-compilation-modes)
            (when-let ((mode (funcall comp-mode-p)))
              (throw 'args (append (list cmd mode) rest)))))
        args)))
  (define-advice project-root (:filter-return (project) abbreviate-project-root)
    (abbreviate-file-name project))
  (defun project-make-predicate-buffer-in-project-p ()
    (let ((project-buffers (project-buffers (project-current))))
      (lambda () (memq (current-buffer) project-buffers))))
  (define-advice project-compile (:around (fn) save-project-buffers-only)
    "Only ask to save project-related buffers."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (project-make-predicate-buffer-in-project-p)))
      (funcall fn)))
  (define-advice recompile
      (:around (fn &optional edit-command) save-project-buffers-only)
    "Only ask to save project-related buffers if inside of a project."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (if (project-current)
               (project-make-predicate-buffer-in-project-p)
             compilation-save-buffers-predicate)))
      (funcall fn edit-command)))
  :config
  (add-to-list 'project-switch-commands
               '(project-dired "Dired"))
  (add-to-list 'project-switch-commands
               '(project-switch-to-buffer "Switch buffer"))
  (add-to-list 'project-switch-commands
               '(project-compile "Compile"))
  (add-to-list 'project-switch-commands
               '(project-save-some-buffers "Save") t))


(use-package projectile
  :ensure projectile
  :diminish
  :defines
  projectile-mode-map
  :functions
  projectile-cleanup-known-projects
  projectile-find-file
  projectile-mode
  projectile-parent
  projectile-project-p
  projectile-project-root
  :hook
  (projectile-after-switch-project . os/node-project-setup)
  :bind
  ("C-c a"   . projectile-ag)
  ("C-c C-o" . projectile-multi-occur)
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1)
  :custom
  (projectile-cache-file
   (expand-file-name ".projectile.cache" os/emacs-tmp-dir))
  (projectile-globally-ignored-files '("TAGS" ".git" ".DS_Store"))
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" os/emacs-tmp-dir))
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-cleanup-known-projects))

(use-package helpful
  :ensure helpful
  :bind
  ("C-c h" . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable)
  ("C-h x" . helpful-command))

;; * GIT
(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . magit-git-commit-insert-branch))
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :preface
  (defun magit-extract-branch-tag (branch-name)
    "Extract branch tag from BRANCH-NAME."
    (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
      (when (string-match-p ticket-pattern branch-name)
        (upcase (replace-regexp-in-string ticket-pattern "\\1: \n" branch-name)))))
  (defun magit-git-commit-insert-branch ()
    "Insert the branch tag in the commit buffer if feasible."
    (when-let ((tag (magit-extract-branch-tag (magit-get-current-branch))))
      (insert tag)
      (forward-char -1))))

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

;; ** GIT-GUTTER
(use-package git-gutter
  :ensure git-gutter
  :diminish ""
  :functions
  global-git-gutter-mode
  :config
  (global-git-gutter-mode t))

(use-package server
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start)))




(use-package recentf
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))))

;; * COMPILE

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :commands (define-compilation-mode)
  :preface
  (cl-defun compile-add-error-syntax
      (mode name regexp &key file line col (level 'error) hyperlink highlight)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (or file (error "Missing value for :file keyword"))
    (or line (error "Missing value for :line keyword"))
    (let ((faces '(compilation-info-face
                   compilation-warning-face
                   compilation-error-face))
          (level (cond ((eq level 'info) 0)
                       ((eq level 'warn) 1)
                       ((eq level 'error) 2)
                       (t (error "Mnsupported level type: %S" level))))
          (mode (symbol-name (or mode 'compilation))))
      (add-to-list (intern (concat mode "-error-regexp-alist")) name)
      (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
                   (list name regexp file line col level hyperlink
                         (list highlight (nth level faces))))))
  (defmacro define-project-compilation-mode (base-name &rest body)
    (declare (indent 1))
    (let* ((name (symbol-name base-name))
           (doc-name (capitalize (replace-regexp-in-string "-compilation$" "" name)))
           (current-project-root (intern (concat name "-current-project")))
           (current-project-files (intern (concat name "-current-project-files")))
           (compilation-mode-name (intern (concat name "-mode"))))
      `(progn
         (defvar ,(intern (concat name "-error-regexp-alist")) nil
           ,(concat "Alist that specifies how to match errors in " doc-name " compiler output.
See `compilation-error-regexp-alist' for more information."))
         (defvar ,(intern (concat name "-error-regexp-alist-alist")) nil
           ,(concat "Alist of values for `" (downcase doc-name) "-compilation-error-regexp-alist'.
See `compilation-error-regexp-alist-alist' for more information."))
         (defvar-local ,current-project-root nil
           ,(concat "Current root of the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (defvar-local ,current-project-files nil
           ,(concat "Current list of files belonging to the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (define-compilation-mode ,compilation-mode-name
           ,(concat doc-name " Compilation")
           ,(concat "Compilation mode for " doc-name " output.")
           (setq-local ,current-project-root (project-current t))
           (setq-local ,current-project-files (project-files ,current-project-root))
           ,@body)
         (provide ',compilation-mode-name)))))

(use-package clojure-compilation-mode
  :no-require
  :after compile
  :config
  (defun clojure-compilation-p ()
    (and (require 'clojure-mode nil 'noerror)
         (clojure-project-root-path)
         'clojure-compilation-mode))
  (add-to-list 'project-compilation-modes 'clojure-compilation-p)
  (defun clojure-compilation--split-classpath (classpath)
    "Split the CLASSPATH string."
    (split-string classpath ":" t "[[:space:]\n]+"))
  (defmemo clojure-compilation--get-project-dependencies-memo
      (command _deps-file _mod-time)
    "Call COMMAND to obtain the classpath string.
DEPS-FILE and MOD-TIME are used for memoization."
    (thread-last
      command
      shell-command-to-string
      clojure-compilation--split-classpath
      (seq-filter (lambda (s) (string-suffix-p ".jar" s)))))
  (defun clojure-compilation--get-lein-project-dependencies (root)
    "Obtain classpath from lein for ROOT."
    (let* ((project-file (expand-file-name "project.clj" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "lein classpath" project-file mod-time)))
  (defun clojure-compilation--get-deps-project-dependencies (root)
    "Obtain classpath from deps for ROOT."
    (let* ((project-file (expand-file-name "deps.edn" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "clojure -Spath" project-file mod-time)))
  (defun clojure-compilation-get-project-dependencies (project)
    "Get dependencies of the given PROJECT.
Returns a list of all jar archives."
    (when (bound-and-true-p tramp-gvfs-enabled)
      (let ((root (project-root project)))
        (cond ((file-exists-p (expand-file-name "deps.edn" root))
               (clojure-compilation--get-deps-project-dependencies root))
              ((file-exists-p (expand-file-name "project.clj" root))
               (clojure-compilation--get-lein-project-dependencies root))))))
  (defvar-local clojure-compilation-project-deps nil
    "List of project's dependencies")
  (defvar-local clojure-compilation-project-deps-mod-time nil
    "Accumulated modification time of all project's libraries")
  (define-project-compilation-mode clojure-compilation
    (require 'tramp-gvfs)
    (setq-local clojure-compilation-project-deps
                (clojure-compilation-get-project-dependencies
                 clojure-compilation-current-project))
    (setq-local clojure-compilation-project-deps-mod-time
                (seq-reduce #'+ (mapcar (lambda (f)
                                          (time-to-seconds
                                           (file-attribute-modification-time
                                            (file-attributes f))))
                                        clojure-compilation-project-deps)
                            0)))
  (defun clojure-compilation--find-file-in-project (file)
    "Check if FILE is part of the currently compiled project."
    (if (file-name-absolute-p file)
        file
      (seq-find
       (lambda (s) (string-suffix-p file s))
       clojure-compilation-current-project-files)))
  (defun clojure-compilation--file-exists-jar-p (jar file)
    "Check if FILE is present in the JAR archive."
    (with-temp-buffer
      (when (zerop (call-process "jar" nil (current-buffer) nil "-tf" jar))
        (goto-char (point-min))
        (save-match-data
          (re-search-forward (format "^%s$" (regexp-quote file)) nil t)))))
  (defmemo clojure-compilation--find-dep-memo
      (file _project _deps-mod-time)
    "Find FILE in current project dependency list.
PROJECT and DEPS-MOD-TIME are used for memoizing the call."
    (when (not (string-empty-p file))
      (seq-find (lambda (d)
                  (clojure-compilation--file-exists-jar-p d file))
                clojure-compilation-project-deps)))
  (defun clojure-compilation--find-dep (file)
    "Find FILE in current project dependency list."
    (clojure-compilation--find-dep-memo
     file
     clojure-compilation-current-project
     clojure-compilation-project-deps-mod-time))
  (defun clojure-compilation-filename ()
    "Function that gets filename from the error message.
If the filename comes from a dependency, try to guess the
dependency artifact based on the project's dependencies."
    (when-let ((filename (substring-no-properties (match-string 1))))
      (or (clojure-compilation--find-file-in-project filename)
          (when-let ((dep (clojure-compilation--find-dep filename)))
            (concat (expand-file-name dep) "/" filename)))))
  (compile-add-error-syntax
   'clojure-compilation 'some-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\) "
   :file #'clojure-compilation-filename
   :line 2 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-warning
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning"
   :file 1 :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-error
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error"
   :file 1 :line 2 :col 3 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-tap
   "^not ok.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-fail
   "^.*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-reflection-warning
   "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-performance-warning
   "^Performance warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-syntax-error
   "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :col 3)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-unit-error
   "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'eastwood-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\):\\([0-9]+\\):"
   :file #'clojure-compilation-filename
   :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1))


(use-package jdecomp
  :ensure t
  :mode ("\\.class\\'" . jdecomp-mode)
  :preface
  (defvar cfr-path
    (file-truename "~/.local/lib/cfr.jar")
    "Path to the cfr Java decompiler library.")
  (defvar fernflower-path
    (file-truename "~/.local/lib/fernflower.jar")
    "Path to the FernFlower library.")
  :when (or (file-exists-p cfr-path)
            (file-exists-p fernflower-path))
  :custom
  (jdecomp-decompiler-type
   (cond ((file-exists-p cfr-path) 'cfr)
         ((file-exists-p fernflower-path) 'fernflower)))
  (jdecomp-decompiler-paths
   `((cfr . ,cfr-path)
     (fernflower . ,fernflower-path))))


;; * NODE JS
(use-package nodejs-repl
  :ensure nodejs-repl
  :commands
  nodejs-repl)

(use-package nvm
  :ensure nvm
  :commands
  nvm-use
  :functions
  nvm--installed-versions
  :custom
  ;; this bit depends on pulling this in from exec-shell,
  ;; which is done in init.el.
  (nvm-dir (getenv "NVM_DIR")))


(use-package js-pkg-mode
  :straight (:type git :local-repo "~/workspace/js-pkg-mode")
  :init (js-pkg-global-mode 1))

(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))


;;;;; Stuff

(setenv "PLENV_ROOT" "/opt/plenv")
(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :demand
  :functions
  exec-path-from-shell-initialize
  :init
  ;; FIXME seeing if this does anything... (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables
   '(
     "CARGO_HOME"
     "GOPATH"
     "GOROOT"
     "MANPATH"
     "NVM_DIR"
     "PATH"
     "PLENV_ROOT"
     "RUSTUP_HOME"
     "SSH_AGENT_PID"
     "SSH_AUTH_SOCK"
     )))

;; * NOTE TAKING

;;; Denote (simple note-taking and file-naming)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
(use-package denote
  :ensure t
  :hook

  ;; Highlight Denote file names in Dired buffers.  Below is the
  ;; generic approach, which is great if you rename files Denote-style
  ;; in lots of places as I do.
  ;;
  ;; If you only want the `denote-dired-mode' in select directories,
  ;; then modify the variable `denote-dired-directories' and use the
  ;; following instead:
  ;;
  ;;  (dired-mode . denote-dired-mode-in-directories)
  ((dired-mode . denote-dired-mode)

   ;; If you use Markdown or plain text files you want to fontify links
   ;; upon visiting the file (Org renders links as buttons right away).
   (text-mode . denote-fontify-links-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c d n" . denote)
    ("C-c d N" . denote-type)
    ("C-c d d" . denote-date)
    ("C-c d z" . denote-signature)      ; "zettelkasten" mnemonic
    ("C-c d s" . denote-subdirectory)
    ("C-c d o" . denote-sort-dired)     ; "order" mnemonic
    ("C-c d j" . denote-journal-extras-new-entry)
    ("C-c d J" . denote-journal-extras-new-or-existing-entry)
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    ("C-c d r" . denote-rename-file)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for
    ;; `org-mode-map', `markdown-mode-map', and/or `text-mode-map'.
    :map text-mode-map
    ("C-c d i" . denote-link)           ; "insert" mnemonic
    ("C-c d I" . denote-add-links)
    ("C-c d b" . denote-backlinks)
    ("C-c d f f" . denote-find-link)
    ("C-c d f b" . denote-find-backlink)
    ;; Also see `denote-rename-file' further above.
    ("C-c d R" . denote-rename-file-using-front-matter)

    ;; I do not bind the Org dynamic blocks, but they are useful:
    ;;
    ;; - `denote-org-extras-dblock-insert-links'
    ;; - `denote-org-extras-dblock-insert-backlinks'
    ;; - `denote-org-extras-dblock-insert-files'
    ;; - `denote-org-extras-dblock-insert-missing-links'

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-link-dired-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-marked-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-file-type 'org)   ; Org is the default, set others here like I do
  ;; If you want to have a "controlled vocabulary" of keywords,
  ;; meaning that you only use a predefined set of them, then you want
  ;; `denote-infer-keywords' to be nil and `denote-known-keywords' to
  ;; have the keywords you need.
  (setq denote-known-keywords '("emacs" "journal" "book" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-excluded-directories-regexp nil)
  (setq denote-date-format nil)         ; read its doc string
  (setq denote-rename-no-confirm t)
  (setq denote-backlinks-show-context nil)
  (setq denote-rename-buffer-format "[D] %t")

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have a literal "[D]"
  ;; followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setq denote-journal-extras-directory nil)    ; use the `denote-directory'
  (setq denote-journal-extras-title-format nil) ; always prompt for title
  (setq denote-journal-extras-keyword "journal")

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))

    ;; This prompts for TITLE, KEYWORDS, and SUBDIRECTORY
    (add-to-list 'org-capture-templates
                 '("N" "New note with prompts (with denote.el)" plain
                   (file denote-last-path)
                   (function
                    (lambda ()
                      (denote-org-capture-with-prompts :title :keywords :signature)))
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))

(use-package elfeed
  :straight '(elfeed :type git :host github :repo "skeeto/elfeed")
  :config
  (setq elfeed-feeds
        '(("https://www.daily.co/blog/rss/" blog ai voice-ai)
          ("https://jackrusher.com/feed.xml" blog clojure))))


;; * AI STUFF
;;;----------------------------------------------------------------

(load (expand-file-name "lisp/emacs-ai.el" user-emacs-directory))
(load (expand-file-name "plugins/ai-project-agent.el" user-emacs-directory))

;;;----------------------------------------------------------------


;; * MONITORING

(use-package wakatime-mode
  :disabled t
  :ensure t
  :diminish ""
  :custom (setq wakatime-api-key os-secret-wakatime-api-key
                wakatime-cli-path "~/.wakatime/wakatime-cli")
  :init (global-wakatime-mode))

(use-package keycast
  :ensure t)


;; * THEMING

(defvar os/load-theme-family 'modus)

(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)
  :hook
  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red))
  :bind
  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (("C-x l" . pulsar-pulse-line)       ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(use-package lin
  :ensure t
  :hook (after-init . lin-global-mode) ; applies to all `lin-mode-hooks'
  :config
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-magenta))

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :bind ("<f8>" . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 2
           :right-divider-width 1
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active ,(if (or (eq os/load-theme-family 'modus)
                                      (eq os/load-theme-family 'standard))
                                  'default
                                'help-key-binding)
           :mode-line-inactive window-divider)))

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-mode))

(use-package modus-themes
  :ensure t
  :defines
  modus-vivendi-tinted-palette-overrides
  modus-operandi-palette-overrides
  :after fontaine
  :commands
  modus-themes-load-theme
  :bind (("<f5>" . modus-themes-toggle)
         ("C-<f5>" . modus-themes-select))
  :config
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-vivendi-tinted-palette-overrides
        '(
          (bg-main "#1b1e26")
          (fg-main "#f0f0f0")))

  (setq modus-operandi-palette-overrides
        `(
          (builtin magenta-warmer)
          (keyword blue)
          (string green-intense)))

  (setq modus-themes-common-palette-overrides
        `(
          ;; From the section "Make the mode line borderless"
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; From the section "Make matching parenthesis more or less intense"
          (bg-paren-match bg-magenta-intense)
          (underline-paren-match fg-main)

          (comment yellow-faint)
          (string green-warmer)


          ,@modus-themes-preset-overrides-warmer))
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package solar
  :config
  (setq calendar-latitude 44.426765
        calendar-longitude 26.102537))

(use-package circadian
  :ensure t
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-vivendi-tinted)))
  :hook (after-init . circadian-setup))


(use-package ef-themes
  :ensure t)

;; * FONT CONFIGURATIONS
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook
  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                   ;; Set last preset or fall back to desired style from `fontaine-presets'.
                   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :bind ("C-c f" . fontaine-set-preset)
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((small
           :default-height 120)
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-height 130)
          (large
           :inherit medium
           :default-height 140)
          (extra-large
           :inherit large
           :default-height 160)
          (live-stream
           :default-family "JetBrains Mono"
           :default-height 150
           :default-weight medium
           :fixed-pitch-family "JetBrains Mono"
           :variable-pitch-family "JetBrains Mono"
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "JetBrains Mono"
           :default-weight regular
           :default-slant normal
           :default-height 120

           :fixed-pitch-family "JetBrains Mono"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "JetBrains Mono"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil

           :tab-line-height 1.0
           :bold-family nil
           :bold-weight extra-bold
           :bold-slant nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-height 1.0

           :line-spacing nil)))
  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

(use-package face-remap
  :ensure nil
  :functions os/enable-variable-pitch
  :bind ( :map ctl-x-x-map
          ("v" . variable-pitch-mode))
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . os/enable-variable-pitch)
  :config
  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (defun os/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))
;;;;; Resize keys with global effect
  :bind
  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C-+" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))

;; * PACKAGE LINT
(use-package package-lint
  :straight '(package-lint :type git :host github :repo "purcell/package-lint"))

;; * LIFE HACKS
(use-package water-reminder
  :ensure nil
  :hook (after-init . water-reminder-mode))

(provide 'init)
;;; init.el ends here

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
