;;; builtins.el -- Customizations of built-in (non-third party) options.  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Large packages (ex. Gnus, ERC, etc.) should get dedicated files but
;;;; for standard stuff where one or two options get frobbed, this is
;;;; the place.

;;; Code:
;;; ANSI-MODE FOR SHELLS
(ansi-color-for-comint-mode-on)

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(defun os/before-save-hook ()
  "My customizations for `before-save-hook'."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook 'os/before-save-hook)


;;; AUTO-SAVES AND BACKUPS
(eval-when-compile (defvar os/emacs-tmp-dir))
(defvar os/backup-dir (concat os/emacs-tmp-dir "saves/" )
  "Place to put file backups.")
(setq auto-save-list-file-prefix (concat os/emacs-tmp-dir "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,os/emacs-tmp-dir t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,os/backup-dir)))
(setq create-lockfiles nil)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

(setq scroll-margin 3)


(add-hook 'prog-mode (lambda () (subword-mode 1)))

;;; BUFFER-MODE
(defun os/set-up-buffer-mode ()
  "My customizations for `Buffer-menu-mode'."
  (setq show-trailing-whitespace nil))
(add-hook 'Buffer-menu-mode-hook 'os/set-up-buffer-mode)


;;; CALENDAR
(defun os/set-up-calendar-mode ()
  "My customizations for `calendar-mode'."
  (setq show-trailing-whitespace nil))
(use-package calendar
  :hook
  (calendar-mode . os/set-up-calendar-mode)
  :custom
  (calendar-mark-holidays-flag t))


;;; CONVERT LINE ENDINGS
;;;; from http://www.emacswiki.org/emacs/EndOfLineTips
(add-hook 'find-file-hook 'os/find-file-check-line-endings)
(defun os/dos-file-endings-p ()
  "Predicate for whether current buffer is in DOS mode."
  (string-match "dos" (symbol-name buffer-file-coding-system)))
(defun os/find-file-check-line-endings ()
  "Convert DOS file to Unix."
  (when (os/dos-file-endings-p)
    (set-buffer-file-coding-system 'undecided-unix)
    (set-buffer-modified-p nil)))


;;; CURSOR
(setq-default cursor-type 'box)

;;; DEBUGGING
(setq-default warning-minimum-level :warning)


(recentf-mode 1)

;;; DESKTOP
(desktop-save-mode -1)

;;; DIRED
;;;; having all-the-icons mode on is pretty, but having the icons in
;;;; the buffer fucks up wdired something fierce. let's advise the
;;;; functions that take us into and out of wdired mode so they remove
;;;; the icons and then put them back.
(defun os/disable-all-the-icons ()
  "Disable all-the-icons."
  (all-the-icons-dired-mode 0))
(defun os/restore-all-the-icons ()
  "Restore all-the-icons."
  (all-the-icons-dired-mode 1))
(advice-add 'wdired-change-to-wdired-mode :before #'os/disable-all-the-icons)
(advice-add 'wdired-finish-edit           :after  #'os/restore-all-the-icons)
(advice-add 'wdired-abort-changes         :after  #'os/restore-all-the-icons)

;;;; http://whattheemacsd.com//setup-dired.el-02.html
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

(use-package dired
  :after (all-the-icons all-the-icons-dired)
  :commands
  dired
  dired-jump
  dired-next-line
  :bind
  ("C-c d"   . dired-jump)
  ("C-c C-d" . dired-jump)
  (:map dired-mode-map
        ("E"                         . wdired-change-to-wdired-mode)
        ([remap beginning-of-buffer] . dired-back-to-top)
        ([remap end-of-buffer]       . dired-jump-to-bottom))
  )


;;; DISABLE / ENABLE
(put 'downcase-region 'disabled nil)
(put 'overwrite-mode  'disabled t)
(put 'upcase-region   'disabled nil)


;;; ELDOC
(use-package eldoc
  :defer t
  :diminish
  :init
  (global-eldoc-mode))


;;; EXECUTABLE-UPON-SAVE MAGIC
;;;; from <http://www.emacswiki.org/cgi-bin/wiki/MakingScriptsExecutableOnSave>
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;; FFAP
(use-package ffap
  :config
  (ffap-bindings))


;;; FONT-LOCK
(use-package font-lock
  :custom
  (jit-lock-stealth-time 5)
  (jit-lock-defer-contextually t)
  (jit-lock-stealth-nice 0.5)
  :config
  (global-font-lock-mode 1)
  (setq-default font-lock-maximum-decoration t)
  (setq-default font-lock-maximum-size nil))


;;; FRAME RESIZE
;;;; see https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/fhicvbj/
(setq frame-inhibit-implied-resize t)


;;; GENERAL INDENTATION RELATED OPTIONS
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq tab-always-indent 'complete)

(defvar modes-for-indentation-munging
  '(c++-mode
    c-mode
    cperl-mode
    emacs-lisp-mode
    go-mode
    js-mode
    js2-mode
    objc-mode
    perl-mode
    python-mode
    rspec-mode
    ruby-mode
    scala-mode
    terraform-mode
    typescript-ts-mode
    tsx-ts-mode
    web-mode)
  "List of modes to set up to do indent-on-paste.
Also remove-leading-whitespace-on-kill-line tricks")

;;;; based on <http://www.emacswiki.org/emacs-en/AutoIndentation>
(defun os/trim-on-kill (&optional _ARG)
  "Remove excess white space when killing newlines in configured modes.
Ignores `ARG'."
  (if (member major-mode modes-for-indentation-munging)
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))
(advice-add 'kill-line :before #'os/trim-on-kill)


;;; GLOBAL AUTO-REVERT
(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  ;; Also auto refresh dired, but be quiet about it
  (global-auto-revert-mode t))


;;; KEYSTROKE ECHO
(setq echo-keystrokes 0.1)


;;; LINE NUMBERS
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;;; MAC STUFF
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))


;;; MESSAGE LOG
(setq message-log-max 5000)


;;; (MENU/SCROLLBAR/TOOLBAR)-MODE
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))


;;; MOUSE WHEEL
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode -1))


;;; NXML-MODE
(use-package nxml-mode
  :mode "\\.\\(rng\\|rss\\|xml\\|xsd\\|xslt\\)\\'"
  :magic "<\\?xml "
  :custom
  (nxml-bind-meta-tab-to-complete-flag nil)
  :init
  (fset 'xml-mode 'nxml-mode))


;;; ORG MODE
(defun os/org-path (path)
  (expand-file-name path org-directory))


(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-goto-interface "outline-path-completion")

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.1)))))

(setq org-directory "~/org/"
      org-agenda-files '("~/org/todo.org")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-ellipsis " ▼ "
      org-log-done 'time
      org-journal-dir "~/org/journal/"
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org"
      org-hide-emphasis-markers nil)
(setq org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "READ(r)" "|"  "DONE(d!)")
        (sequence "|" "WAIT(w)" "BACK(b)")))

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "orange red" :weight bold))
        ("WAIT" . (:foreground "HotPink2" :weight bold))
        ("BACK" . (:foreground "MediumPurple3" :weight bold))))

;; Configure common tags
(setq org-tag-alist
      '((:startgroup)
                                        ; Put mutually exclusive tags here
        (:endgroup)
        ("@home" . ?H)
        ("@work" . ?W)
        ("batch" . ?b)
        ("followup" . ?f)))

;; Use directional window movement (S-<left> to move to the left window)
(windmove-default-keybindings)

(setq org-agenda-window-setup 'current-window)

;; Make done tasks show up in the agenda log
(setq org-log-done 'time)
(setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")
(setq org-agenda-custom-commands
      `(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting for")
                 (org-agenda-text-search-extra-files nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                 (org-agenda-text-search-extra-files nil)))))

        ("n" "Next Tasks"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))))
(setq org-capture-templates
      `(("t" "Todo" entry (file+headline "~/org/todo.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :prepend t :empty-lines 1)

        ("j" "Journal Entries")
        ("je" "General Entry" entry
         (file+olp+datetree ,(os/org-path "Journal.org"))
         "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :prepend t
         :empty-lines 1)
        ("jt" "Task Entry" entry
         (file+olp+datetree ,(os/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
        ("jj" "Journal" entry
         (file+olp+datetree ,(os/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)))


;;; PAREN MATCH
(use-package paren
  :defines show-paren-style
  :custom
  (show-paren-style 'parenthesis)
  :config
  (setq show-paren-context-when-offscreen 'overlay)
  (show-paren-mode t))


;;; PS PRINT
(setq ps-print-color-p nil)


;;; SAVE-HIST
(use-package savehist
  :custom
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-file (expand-file-name "savehist" os/emacs-tmp-dir))
  :config
  (savehist-mode t))


;;; SAVEPLACE
(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" os/emacs-tmp-dir))
  :config
  (save-place-mode))


;;; SCRATCH-BUFFER
(setq initial-scratch-message ";; this is my emacs. there are many like it, but this one is mine.\n\n")
(defalias 'os/create-scratch-buffer 'scratch-buffer)


;;; SERVER
(server-start)


;;; SIZE INDICATION MODE
(size-indication-mode t)




;;; TEXT-MODE
(declare-function filladapt-mode "fillapdapt")

(defun os/set-up-text-mode ()
  "My customizations for `text-mode'."
  (require 'filladapt)
  (auto-fill-mode 1)
  (filladapt-mode 1))

(use-package text-mode
  :hook
  (text-mode . os/set-up-text-mode))


;;; TITLE BARS
(setq frame-title-format "<%b> == (%f) [mode: %m]")


;;; TRANSIENT MARK MODE
(transient-mark-mode 1)


;;; TRASH
(setq delete-by-moving-to-trash t)


;;; UNIQUIFY
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t))


;;; UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;; VC
(setq vc-follow-symlinks t)


;;; VIEW MODE
;;;; use view-mode in read-only buffers (https://karthinks.com/software/batteries-included-with-emacs/)
(setq view-read-only t)

;;; YANK
(setq-default mouse-yank-at-point t)


;;; YES-OR-NO
(defalias 'yes-or-no-p 'y-or-n-p)


(provide 'builtins)
;;; builtins.el ends here
