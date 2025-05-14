;;; setup-core.el --- Config related to core emacs functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package delight
  :ensure t)

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

  (provide 'defaults))


(use-package windmove
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings ))

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
         ("C-h C-f" . describe-face))
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

(use-package help
  :custom
  (help-window-select t))

(use-package paren
  :hook (prog-mode . show-paren-mode))

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
  :custom (browse-url-browser-function #'browse-url-default-browser))


(use-package centered-window :ensure t)


(use-package repeat
  :hook (after-init . repeat-mode))

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

(use-package server
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start)))

(provide 'setup-core)
;;; setup-core.el ends here
