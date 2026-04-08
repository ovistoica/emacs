;; Auto refresh buffers
(use-package autorevert
  :config (global-auto-revert-mode 1))

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keybinding prefixes faster
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Shift is more useful as a modifier
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(setopt use-short-answers t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; 80 chars is a good width
(set-default 'fill-column 80)

;; Remove text in active region if inserting text
(use-package delsel
  :defer 1
  :config (delete-selection-mode 1))

;; Always display column numbers
(setq column-number-mode t)
(setq line-number-mode t)

;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :defer 1 ;; Loads after 1 second of idle time.
  :config (recentf-mode 1)
  :custom (recentf-max-saved-items 1000))  ;; just 20 is too recent

;; Undo/redo window configuration with C-c <left>/<right>
(use-package winner
  :ensure t
  :config (winner-mode +1))

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines nil)

;; Easily navigate sillycased words
(use-package subword
  :defer 1
  :config (global-subword-mode 1)
  :diminish subword-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
  :ensure nil
  :defer 2 ;; Loads after 2 seconds of idle time.
  :custom (uniquify-buffer-name-style 'forward))

;; Start Emacs server for emacsclient
(use-package server
  :commands server-running-p
  :defer 2
  :config (unless (server-running-p)
            (message "Starting emacs server...")
            (server-start)))

;; Show more than 4 levels when evaling expressions
(setq eval-expression-print-level 100)

(defun my-create-non-existent-directory ()
  "Offer to create parent directories if they don't exist.
Credit:
http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/"
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Try first to go to file under point
(ffap-bindings)

;; Don't zoom individual buffers
(global-unset-key (kbd "C-x C-+"))
(global-unset-key (kbd "s-t"))

;; Disable mouse zooming
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't write lock-files, I'm the only one here
(setq create-lockfiles nil)

;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; No electric indent
(setq electric-indent-mode nil)

;; Automatically prompt for sudo in write protected files
(use-package auto-sudoedit
  :defer 1
  :config (auto-sudoedit-mode 1))

;; Make calendars start on Monday
(setq calendar-week-start-day 1)

;; Bucharest
(setq calendar-latitude 44.4)
(setq calendar-longitude 26.1)
(setq calendar-location-name "Bucharest, Romania")
(setq calendar-time-zone-style 'numeric)

(setq native-comp-async-report-warnings-errors 'silent)

;; If you don’t edit right-to-left languages (Arabic, Hebrew, etc.), Emacs is
;; doing a bunch of work on every redisplay cycle for nothing. These settings
;; tell Emacs to assume left-to-right text everywhere and skip the bidirectional
;; parenthesis algorithm:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Emacs normally fontifies (syntax-highlights) text even while you’re actively
;; typing. This can cause micro-stutters, especially in tree-sitter modes or
;; large buffers. One setting fixes it:
(setq redisplay-skip-fontification-on-input t)

;; The default read-process-output-max is 64KB, which is still quite
;; conservative. Modern LSP servers like rust-analyzer or clangd routinely send
;; multi-megabyte responses. Bumping this reduces the number of read calls Emacs
;; has to make:
(setq read-process-output-max (* 4 1024 1024)) ; 4MB


;; If you have several windows visible, Emacs draws a cursor in each of them –
;; even the ones you’re not working in. It also highlights selections in
;; non-focused windows. Two settings to stop that:
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Here’s a scenario: you copy a URL from your browser, switch to Emacs, kill a
;; line with C-k, and then try to yank the URL you copied earlier with C-y.
;; Gone. The kill replaced it on the clipboard. This setting makes Emacs save
;; the existing clipboard content into the kill ring before overwriting it:
(setq save-interprogram-paste-before-kill t)

;; Kill the same line three times and you get three identical entries in the
;; kill ring, wasting slots. This deduplicates them:
(setq kill-do-not-save-duplicates t)

;; Add it and you get clipboard history that survives restarts:
(require 'savehist)
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))

;; One thing to watch out for: the kill ring can accumulate text properties
;; (fonts, overlays, etc.) that bloat the savehist file. Strip them before
;; saving:
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))


;; The mark ring is one of Emacs’s most underused navigation features. Every
;; time you jump somewhere – isearch, M-<, M->, goto-line, imenu, and many more
;; – Emacs pushes your old position onto the mark ring. C-u C-SPC pops it,
;; jumping you back.

;; The annoyance: you need C-u C-SPC every single time. With this setting, after
;; the first C-u C-SPC you can keep pressing just C-SPC to continue popping:
(setq set-mark-command-repeat-pop t)


;; save-place-mode is great – it remembers where you were in each file and jumps
;; back there when you reopen it. The problem is that it can leave your cursor
;; on the last visible line of the window, which is disorienting. This advice
;; recenters the view after the jump:
(use-package saveplace
  :defer 3
  :init
  (save-place-mode 1)
  :custom
  (save-place-ignore-files-regexp
   "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|elpa\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")
  (save-place-file (concat user-emacs-directory ".my-saved-places"))
  (save-place-forget-unreadable-files t))

(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))


;; When you press C-h f or C-h v, Emacs opens the help buffer but leaves your
;; cursor in the original window. You almost always want to read the help right
;; away, so you end up pressing C-x o every single time. This fixes it:
(setq help-window-select t)

;; If you create a file that starts with #! (a shebang line), it should be
;; executable. But you always forget to chmod +x it, run the script, get
;; “Permission denied”, curse, go back, chmod, try again. This hook does it
;; automatically:
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; re-builder (M-x re-builder) is an interactive tool for developing regexps –
;; you type a pattern and see matches highlighted live in the target buffer. The
;; problem is the default syntax: read. In read syntax, you have to
;; double-escape everything, so a word boundary is \\< and a group is \\(...\\).
;; It’s the regexp equivalent of trying to type with oven mitts on. Switch to
;; string syntax and things look like normal Emacs regexps:
(setq reb-re-syntax 'string)


;; Ever had Emacs freeze for a few seconds when you ran find-file-at-point (or a
;; command that uses it internally)? If the text under point looks like a
;; hostname – say, something.com in a comment – ffap tries to ping it to check
;; if it’s reachable. On a slow or firewalled network, that’s a multi-second
;; hang.
(setq ffap-machine-p-known 'reject)

(provide 'sane-defaults)
;;; sane-defaults.el ends here
