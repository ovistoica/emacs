;;; packages.el --- Use-package configuration -*- lexical-binding: t -*-

;; Note: Package system is already initialized in early-init.el

;; Add custom packages directory to load-path
(setq packages-dir (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path packages-dir)

;; Bootstrap use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Set CC compiler path only on macOS for native compilation
(when (eq system-type 'darwin)
  (setenv "CC" "/opt/homebrew/bin/gcc-15"))

(provide 'packages)
