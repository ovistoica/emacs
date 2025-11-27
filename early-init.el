;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;
;; This file is loaded before the package system and GUI is initialized.
;;

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously by setting
;; `native-comp-jit-compilation' to t.
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated

;; Startup optimizations
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

;; Increase garbage collection threshold during startup (100 MB)
;; This will be restored to a lower value after startup completes
(setq gc-cons-threshold (* 1024 1024 100))

;; Disable file-name-handler-alist during startup for faster file operations
;; This will be restored after startup completes
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Package management setup
;; Disable automatic package loading at startup (we'll do it manually)
(setq package-enable-at-startup nil)

;; Configure package archives early
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize package system
(package-initialize)

;; Enable native compilation for packages
(setq package-native-compile t)

;;; early-init.el ends here
