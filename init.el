;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Redirect .elc files to a separate directory
(defun my/setup-byte-compile-destination ()
  "Configure byte compilation to output .elc files to a separate directory."
  (let ((elc-dir (expand-file-name "elc/" user-emacs-directory)))
    ;; Create the elc directory if it doesn't exist
    (unless (file-directory-p elc-dir)
      (make-directory elc-dir t))

    ;; Set the byte-compile destination function
    (setq byte-compile-dest-file-function
          (lambda (source)
            (let* ((relative-path (file-relative-name source user-emacs-directory))
                   (dest-path (expand-file-name relative-path elc-dir))
                   (dest-dir (file-name-directory dest-path)))
              ;; Ensure the destination directory exists
              (unless (file-directory-p dest-dir)
                (make-directory dest-dir t))
              ;; Return the .elc file path
              (concat (file-name-sans-extension dest-path) ".elc"))))))

;; Call immediately
(my/setup-byte-compile-destination)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Add settings to load-path
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

;; Optimize startup of emacs
(require 'fast-startup)

;; Keep emacs Custom-settings in separate file, not appended to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Configure the package manager
(require 'packages)

;; Set up appearance early
(require 'appearance)


;; Configure Emacs for OSX
(when (eq system-type 'darwin)
  (require 'romanian-mac))

;; Configure Emacs for Linux
(when (eq system-type 'gnu/linux)
  (require 'linux-setup))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Set up tooling for the rest of the configuration
(require 'tooling)

;; Add utilities
(require 'navigation)
(require 'editing)
(require 'buffers)
(require 'windows)
(require 'extra-keybindings)
(require 'indented-yank)

(require 'setup-significant-other)

;; Load all packages
(dolist (file (directory-files packages-dir t "^[^#].*el$"))
  (when (file-regular-p file)
    (load file)))

(require 'work)
