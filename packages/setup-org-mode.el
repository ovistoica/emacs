(require 'romanian-mac)

(use-package org
  :ensure nil
  :defer t

  :custom
  (org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "PROJECT(p)" "|" "DONE(d)")))

  (org-todo-keyword-faces
   '(("DONE" . (:foreground "green" :weight bold))
     ("WAITING" . (:foreground "orange" :weight bold))
     ("NEXT" . (:foreground "green" :weight bold))
     ("PROJECT" . (:foreground "blue" :weight bold))))

  (org-use-fast-todo-selection t)

  (org-log-done 'time)

  :bind (:map org-mode-map
              ("M-+" . org-shiftright)
              ("C-S-<down>" . org-metadown)
              ("C-S-<up>" . org-metaup))

  :hook ((org-mode . my/setup-romanian-diacritics))

  :config

  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)

  (setq org-directory "~/Dropbox/org")

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/working-memory.org" "INBOX")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  (setq org-feed-alist
        '(("Slashdot"
           "https://rss.slashdot.org/Slashdot/slashdot"
           "~/Dropbox/org/feeds.org" "Slashdot Entries")))

  ;; Update [/] cookies after a selection of commands

  (--each '(org-yank
            org-kill-line
            kill-whole-line
            duplicate-current-line-or-region)
    (advice-add it :after 'my/org-update-parent-cookie)))

(defun my/org-update-parent-cookie (&rest _)
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

(provide 'setup-org-mode)
