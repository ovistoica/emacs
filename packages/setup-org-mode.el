;;; setup-org-mode.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Org mode including TODO keywords, capture templates,
;; agenda files, and RSS feeds.

;;; Code:

(require 'dash)

;; Silence byte-compiler warnings for org variables set in :config
(defvar org-indent-mode-turns-on-hiding-stars)
(defvar org-capture-templates)
(defvar org-feed-alist)

;; Declare org functions used outside of org-mode context
(declare-function org-back-to-heading "org")
(declare-function org-update-parent-todo-statistics "org")

;; Load macOS-specific configuration if on macOS
(when (eq system-type 'darwin)
  (require 'romanian-mac))

(use-package org
  :ensure nil
  :defer t

  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w)" "EVENT(e)" "|" "DONE(d)")))

  (org-todo-keyword-faces
   '(("DONE" . (:foreground "green" :weight bold))
     ("WAIT" . (:foreground "orange" :weight bold))
     ("EVENT" . (:foreground "blue" :weight bold))))

  (org-use-fast-todo-selection t)

  (org-log-done 'time)

  :bind (:map org-mode-map
              ("M-+" . org-shiftright)
              ("C-S-<down>" . org-metadown)
              ("C-S-<up>" . org-metaup))

  :hook (org-mode . auto-fill-mode)

  :config

  ;; Disable to support integration of window-mode with org schedule
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)

  ;; Don't do literal modifications to the file
  (setq org-adapt-indentation nil)

  ;;Use indent mode but keep the stars visible
  (setq org-indent-mode-turns-on-hiding-stars nil)

  (setq org-directory "~/Dropbox/org")

  (setq org-capture-templates
        '(("a" "Affirmation" entry (file+olp+datetree "~/Dropbox/org/affirmations.org")
           "* I am %?\n  %U\n\nCurrent affirmations:\n- I am a clojure startup owner of 1M ARR\n- I own a medium house in the country-side \n- I am a man of 70kgs\n- I am a husband of a happy family" :empty-lines 1)
          ("b" "Bookmark" entry (file+headline "~/Dropbox/org/bookmarks.org" "Bookmarks")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
          ("s" "Scheduled TODO" entry (file+headline "~/Dropbox/org/working-memory.org" "INBOX")
           "* EVENT %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
          ("e" "Event" entry (file+headline "~/Dropbox/org/working-memory.org" "Events")
           "* %? \n%^t\n %U\n  %a" :empty-lines 1)
          ("d" "Deadline" entry (file+headline "~/Dropbox/org/working-memory.org" "INBOX")
           "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
          ("t" "Todo" entry (file+headline "~/Dropbox/org/working-memory.org" "INBOX")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry  (file+olp+datetree "~/Dropbox/org/journal.org")
           "* %?\nEntered on %U\n  %i\n ")
          ("p" "Social Post" entry (file+headline "~/Dropbox/org/social-posts.org" "Posts")
           "* %?\n  %U" :empty-lines 1)))

  (setq org-agenda-files '("working-memory.org" "journal.org" "inbox.org" "calendar-beorg.org"))

  (setq org-feed-alist
        '(("Slashdot"
           "https://rss.slashdot.org/Slashdot/slashdot"
           "~/Dropbox/org/feeds.org" "Slashdot Entries")
          ("Clojure deref"
           "https://clojure.org/feed.xml"
           "~/Dropbox/org/feeds.org" "Clojure Deref Entries")))

  ;; Update [/] cookies after a selection of commands

  (--each '(org-yank
            org-kill-line
            kill-whole-line
            duplicate-current-line-or-region)
    (advice-add it :after 'my/org-update-parent-cookie))

  (require 'ox-gfm nil t))

(defun my/org-update-parent-cookie (&rest _)
  "Update parent TODO statistics cookie after certain operations."
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (forward-char 1)
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))
    (save-excursion
      (ignore-errors
        (forward-char -1)
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))


(defun my/markdown-to-org-region (start end)
  "Convert Markdown formatted text in region (START, END) to Org.

This command requires that pandoc (man page `pandoc(1)') be
installed."
  (interactive "r")
  (shell-command-on-region
   start end
   "pandoc -f markdown -t org --wrap=preserve" t t))


;; Add Romanian diacritics hook on macOS
(when (eq system-type 'darwin)
  (add-hook 'org-mode-hook 'my/setup-romanian-diacritics))

(provide 'setup-org-mode)
;;; setup-org-mode.el ends here
