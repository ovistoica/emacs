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

;; Set up appearance early
(require 'appearance)

;; Set up Straight (for packages on github) - MUST come before packages.el
(require 'setup-straight)

;; Configure the package manager
(require 'packages)

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
