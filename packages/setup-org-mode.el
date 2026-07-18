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
(declare-function org-yank "org")
(declare-function org-display-inline-images "org")

;; Load macOS-specific configuration if on macOS
(when (eq system-type 'darwin)
  (require 'romanian-mac))


(use-package org
  :ensure nil
  :defer t

  :functions (modus-themes-get-color-value)

  :preface
  (defvar my/org-todo-face-colors nil
    "Alist of (FACE . MODUS-COLOR-SYMBOL) pairs.
Populated by `set-todo-face' and consumed by `my/org-sync-todo-faces'
to color Org TODO keyword faces from the active Modus theme palette.")

  (defvar my/org-todo-keyword-faces nil
    "Alist of (KEYWORD . FACE) pairs, in `org-todo-keyword-faces' format.
Populated by `set-todo-face' and assigned to `org-todo-keyword-faces'.")

  (defmacro set-modus-todo-face (keyword color)
    "Define and register a face for the Org TODO KEYWORD.
KEYWORD is a string such as \"PENDING_QA\".  COLOR is a Modus theme
color symbol, looked up later via `modus-themes-get-color-value'.

Derives a face name from KEYWORD (lower-cased, underscores turned
into hyphens, prefixed with `my/org-todo-'), defines it via
`defface' with an auto-generated docstring, and registers it in
`my/org-todo-face-colors' and `my/org-todo-keyword-faces'."
    (declare (indent defun))
    (let ((face (intern (concat "my/org-todo-"
                                (downcase (replace-regexp-in-string
                                           "_" "-" keyword))))))
      `(progn
         (defface ,face '((t :weight bold))
           ,(format "Face for the %s Org keyword." keyword))
         (setf (alist-get ',face my/org-todo-face-colors) ',color)
         (setf (alist-get ,keyword my/org-todo-keyword-faces nil nil #'equal)
               ',face))))

  (set-modus-todo-face "TODO" red-faint)
  (set-modus-todo-face "INPROGRESS" yellow-warmer)
  (set-modus-todo-face "REVIEW" blue)
  (set-modus-todo-face "PENDING_QA" cyan)
  (set-modus-todo-face "NEXT" red-intense)
  (set-modus-todo-face "PROJ" magenta)
  (set-modus-todo-face "WAIT" yellow)
  (set-modus-todo-face "SOMEDAY" fg-alt)
  (set-modus-todo-face "DONE" bg-added-fringe)

  (defun my/org-sync-todo-faces (&rest _)
    "Color the Org TODO keyword faces from the active Modus theme palette."
    (when (fboundp 'modus-themes-get-color-value)
      (dolist (spec my/org-todo-face-colors)
        (set-face-attribute (car spec) nil
                            :foreground (modus-themes-get-color-value (cdr spec))))))

  (defun my/org-clipboard-has-image-p ()
    "Return non-nil if the clipboard contains image data."
    (when-let* ((targets (ignore-errors
                           (gui-get-selection 'CLIPBOARD 'TARGETS))))
      (seq-some (lambda (type)
                  (and (symbolp type)
                       (string-match-p "^image/" (symbol-name type))))
                (if (vectorp targets) (append targets nil) targets))))

  (defun my/org-yank-dwim ()
    "Yank a clipboard image as a file link, or fall back to `org-yank'.
When the clipboard holds an image, save it to disk via `yank-media'
\(see `org-yank-image-save-method'), insert a link at point, and
refresh inline image display.  Otherwise behave like `org-yank'."
    (interactive)
    (if (and (display-graphic-p)
             (fboundp 'yank-media)
             (my/org-clipboard-has-image-p))
        (progn
          (call-interactively #'yank-media)
          (org-display-inline-images))
      (call-interactively #'org-yank)))

  :custom
  (org-todo-keywords
   '((sequence "NEXT(n)"  "INPROGRESS(i)" "TODO(t)" "REVIEW(r)" "PROJ(p)" "WAIT(w)" "SOMEDAY(s)"
               "|" "DONE(d)" "CANCELED(c)")))

  (org-todo-keyword-faces my/org-todo-keyword-faces)

  (org-use-fast-todo-selection t)

  (org-log-done 'time)

  ;; Log a "State \"Y\" from \"X\"" timestamp line into a LOGBOOK drawer for
  ;; every keyword transition (see the "!" flags above), not just on DONE.
  (org-log-into-drawer t)

  ;; Save clipboard-pasted images to an images/ dir next to the org file
  ;; and insert a relative file: link.  Set to 'attach to use org-attach
  ;; per-heading directories instead.
  (org-yank-image-save-method "images/")

  :bind (:map org-mode-map
              ("C-y" . my/org-yank-dwim)
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
          ("t" "Todo" entry (file+headline "~/Dropbox/org/working-memory.org" "INBOX")
           "* TODO %?\n  %i\n  %a")
          ("s" "Shopping item" entry (file "~/Dropbox/org/inbox.org")
           "* %? :SHOPPING:\n")
          ("w" "Work Todo" entry (file+headline "~/Dropbox/org/master-list.org" "INBOX")
           "* TODO %?\n  %i\n  %a")
          ;; ("j" "Journal" entry  (file+olp+datetree "~/Dropbox/org/journal.org")
          ;;  "* %?\nEntered on %U\n  %i\n ")
          ("u" "Eng Update" entry  (file+olp+datetree "~/Dropbox/org/engineering-update.org")
           "* %(let* ((day-num (string-to-number (format-time-string \"%w\"))) (days '(\"Sun\" \"Mon\" \"Tue\" \"Wed\" \"Thu\" \"Fri\" \"Sat\"))) (concat (nth day-num days) (format-time-string \" %b %d\"))) Update:\n\n%?")
          ;; ("p" "Social Post" entry (file+headline "~/Dropbox/org/social-posts.org" "Posts")
          ;;  "* %?\n  %U" :empty-lines 1)
          ))

  (setq org-agenda-files '("working-memory.org" "journal.org" "inbox.org" "calendar-beorg.org" "master-list.org" "reminders-beorg.org"))

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

(use-package org-pomodoro
  :defer t)

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

;; Add Romanian diacritics hook on macOS
(when (eq system-type 'darwin)
  (add-hook 'org-mode-hook 'my/setup-romanian-diacritics))

(require 'org-slack-export)

(provide 'setup-org-mode)
;;; setup-org-mode.el ends here
