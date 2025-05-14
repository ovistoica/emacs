;; -*- lexical-binding: t -*-
;;; init.el --- My personal emacs configuration.
;;; Author: Ovidiu Stoica
;;; Commentary:
;;; Currently written for Emacs 30.
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


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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

(setq config-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path config-dir)

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

(use-package emacs
  :config
  (ffap-bindings)
  (push (expand-file-name "plugins/" user-emacs-directory) load-path)
  (push (expand-file-name "lisp/" user-emacs-directory)  load-path))

;; ** MAC Stuff
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))


(require 'setup-functions)
(require 'setup-core)
(require 'setup-lint)
(require 'setup-treesitter)
(require 'setup-completions)
(require 'setup-lsp)
(require 'setup-consult)
(require 'setup-org)
(require 'setup-navigation-editing)
(require 'setup-web)
(require 'setup-project)
(require 'setup-clojure)
(require 'setup-notetaking)
(require 'setup-shell)
(require 'setup-git)
(require 'setup-languages)
(require 'setup-icons)
(require 'setup-ai)
(require 'setup-popper)
(require 'setup-theme)
(require 'setup-formatting)


(provide 'init)
;;; init.el ends here
