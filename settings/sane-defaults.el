;; Auto refresh buffers
(use-package autorevert
  :defer 2
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
  :defer 1
  :config (winner-mode 1))

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

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
  :defer 2
  :config (unless (server-running-p)
            (server-start)))

;; Show more than 4 levels when evaling expressions
(setq eval-expression-print-level 100)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
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

(provide 'sane-defaults)
