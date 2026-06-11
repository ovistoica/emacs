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
(declare-function org-sort-entries "org")
(declare-function org-get-todo-state "org")
(declare-function org-up-heading-safe "org")

;; Load macOS-specific configuration if on macOS
(when (eq system-type 'darwin)
  (require 'romanian-mac))


(use-package org
  :ensure nil
  :defer t

  :functions (modus-themes-get-color-value)

  :preface
  (defface my/org-todo-todo '((t :weight bold))
    "Face for the TODO Org keyword.")
  (defface my/org-todo-inprogress '((t :weight bold))
    "Face for the INPROGRESS Org keyword.")
  (defface my/org-todo-review '((t :weight bold))
    "Face for the REVIEW Org keyword.")
  (defface my/org-todo-pending-qa '((t :weight bold))
    "Face for the PENDING_QA Org keyword.")
  (defface my/org-todo-wait '((t :weight bold))
    "Face for the WAIT Org keyword.")
  (defface my/org-todo-event '((t :weight bold))
    "Face for the EVENT Org keyword.")
  (defface my/org-todo-done '((t :weight bold))
    "Face for the DONE Org keyword.")

  (defun my/org-sync-todo-faces (&rest _)
    "Color the Org TODO keyword faces from the active Modus theme palette."
    (when (fboundp 'modus-themes-get-color-value)
      (dolist (spec '((my/org-todo-todo       . red)
                      (my/org-todo-inprogress . yellow-warmer)
                      (my/org-todo-review     . blue)
                      (my/org-todo-pending-qa . cyan)
                      (my/org-todo-wait       . yellow)
                      (my/org-todo-event      . blue-warmer)
                      (my/org-todo-done       . bg-added-fringe)))
        (set-face-attribute (car spec) nil
                            :foreground (modus-themes-get-color-value (cdr spec))))))

  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i)" "REVIEW(r)" "PENDING_QA(p)"
               "WAIT(w)" "EVENT(e)" "|" "DONE(d)" "NOTABUG(n)")))


  (org-todo-keyword-faces
   '(("TODO" . my/org-todo-todo)
     ("INPROGRESS" . my/org-todo-inprogress)
     ("REVIEW" . my/org-todo-review)
     ("PENDING_QA" . my/org-todo-pending-qa)
     ("WAIT" . my/org-todo-wait)
     ("EVENT" . my/org-todo-event)
     ("DONE" . my/org-todo-done)))

  (org-use-fast-todo-selection t)

  (org-log-done 'time)

  :bind (:map org-mode-map
              ("M-+" . org-shiftright)
              ("C-S-<down>" . org-metadown)
              ("C-S-<up>" . org-metaup))

  :hook ((org-mode . auto-fill-mode)
         (enable-theme-functions . my/org-sync-todo-faces))

  :config

  ;; Color the TODO keyword faces from the current theme now (the hook keeps
  ;; them in sync on later theme switches).
  (my/org-sync-todo-faces)

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
        '(;; ("a" "Affirmation" entry (file+olp+datetree "~/Dropbox/org/affirmations.org")
          ;;  "* I am %?\n  %U\n\nCurrent affirmations:\n- I am a clojure startup owner of 1M ARR\n- I own a medium house in the country-side \n- I am a man of 70kgs\n- I am a husband of a happy family" :empty-lines 1)
          ("b" "Bookmark" entry (file+headline "~/Dropbox/org/bookmarks.org" "Bookmarks")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
          ;; ("t" "Todo" entry (file+headline "~/Dropbox/org/working-memory.org" "INBOX")
          ;;  "* TODO %?\n  %i\n  %a")
          ;; ("j" "Journal" entry  (file+olp+datetree "~/Dropbox/org/journal.org")
          ;;  "* %?\nEntered on %U\n  %i\n ")
          ("u" "Eng Update" entry  (file+olp+datetree "~/Dropbox/org/engineering-update.org")
           "* %(let* ((day-num (string-to-number (format-time-string \"%w\"))) (days '(\"Sun\" \"Mon\" \"Tue\" \"Wed\" \"Thu\" \"Fri\" \"Sat\"))) (concat (nth day-num days) (format-time-string \" %b %d\"))) Update:\n\n%?")
          ;; ("p" "Social Post" entry (file+headline "~/Dropbox/org/social-posts.org" "Posts")
          ;;  "* %?\n  %U" :empty-lines 1)
          ))



  (setq org-agenda-files '("working-memory.org" "journal.org" "inbox.org" "calendar-beorg.org" "master-list.org"))

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


(use-package ox-gfm
  :ensure t)

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

;;; Auto-sort subtree by TODO state after state changes

(defcustom my/org-todo-sort-order
  '("INPROGRESS" "TODO" "WAIT" "EVENT" "REVIEW" "DONE" "NOTABUG")
  "Sort priority for TODO keywords; earlier entries sort first.
Keywords absent from this list sink to the bottom."
  :type '(repeat string)
  :group 'org)
;; Ensure the order is always applied, even if defcustom skipped the init value
(setq my/org-todo-sort-order '("INPROGRESS" "TODO" "WAIT" "EVENT" "REVIEW" "DONE" "NOTABUG"))

(defun my/org-todo-sort-key ()
  "Return a numeric sort key for the current heading's TODO state."
  (let* ((state (org-get-todo-state))
         (pos (and state (cl-position state my/org-todo-sort-order :test #'equal))))
    (or pos (length my/org-todo-sort-order))))

(defun my/org-sort-parent-subtree ()
  "Sort direct children of the parent heading by `my/org-todo-sort-order'.
Narrows to the parent subtree first so only siblings of the changed
entry are reordered, leaving other top-level headings untouched."
  (when (and (derived-mode-p 'org-mode)
             (org-get-todo-state))
    (save-excursion
      (condition-case nil
          (when (org-up-heading-safe)
            (save-restriction
              (org-narrow-to-subtree)
              (org-sort-entries nil ?f #'my/org-todo-sort-key #'<)))
        (error nil)))))

(add-hook 'org-after-todo-state-change-hook #'my/org-sort-parent-subtree)

(require 'org-slack-export)

(provide 'setup-org-mode)
;;; setup-org-mode.el ends here
