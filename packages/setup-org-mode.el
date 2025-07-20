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


(defun my/markdown-to-org-region (start end)
  "Convert Markdown formatted text in region (START, END) to Org.

This command requires that pandoc (man page `pandoc(1)') be
installed."
  (interactive "r")
  (shell-command-on-region
   start end
   "pandoc -f markdown -t org --wrap=preserve" t t))

(defun my/org-de-autofill (&optional start end)
  "Remove autofill formatting from region or entire buffer.
If a region is active, process only the region (START, END).
Otherwise, process the entire buffer."
  (interactive (when (use-region-p)
                 (list (region-beginning) (region-end))))
  (let* ((use-region (and start end))
         (beg (or start (point-min)))
         (end-pos (or end (point-max)))
         (text (buffer-substring beg end-pos))
         (processed-text
          (with-temp-buffer
            (insert text)
            (goto-char (point-min))
            ;; Join lines that were broken by autofill
            ;; Look for lines that don't end with sentence endings or intentional breaks
            (while (not (eobp))
              (let ((line-end (line-end-position)))
                (when (and (not (eobp))
                           (< line-end (point-max))
                           (not (looking-at ".*[.!?:]\\s-*$"))  ; Not ending with punctuation
                           (not (looking-at ".*\\s-\\s-$"))      ; Not ending with double space
                           (not (looking-at "^\\s-*$"))         ; Not empty line
                           (not (looking-at "^\\*+\\s-"))       ; Not org heading
                           (not (looking-at "^#\\+"))           ; Not org option line
                           (save-excursion
                             (forward-line 1)
                             (and (not (looking-at "^\\s-*$"))  ; Next line not empty
                                  (not (looking-at "^\\*+\\s-")) ; Next line not heading
                                  (not (looking-at "^#\\+")))))  ; Next line not option
                  (end-of-line)
                  (delete-char 1)  ; Remove newline
                  (when (looking-at "\\s-+")
                    (delete-region (point) (match-end 0)))  ; Remove extra spaces
                  (insert " "))  ; Add single space
                (forward-line 1)))
            (buffer-string))))
    ;; Replace the content
    (save-excursion
      (delete-region beg end-pos)
      (goto-char beg)
      (insert processed-text))))

(defun my/org-auto-fillify (&optional start end)
  "Apply autofill formatting to region or entire buffer.
If a region is active, process only the region (START, END).
Otherwise, process the entire buffer."
  (interactive (when (use-region-p)
                 (list (region-beginning) (region-end))))
  (let* ((use-region (and start end))
         (beg (or start (point-min)))
         (end-pos (or end (point-max)))
         (text (buffer-substring beg end-pos))
         (processed-text
          (with-temp-buffer
            (org-mode)  ; Use org-mode for proper syntax handling
            (insert text)
            (auto-fill-mode 1)
            (setq fill-column (or (and (boundp 'org-fill-column) org-fill-column) 
                                  fill-column 
                                  70))
            (goto-char (point-min))
            ;; Process line by line, being careful with org syntax
            (while (not (eobp))
              (cond
               ;; Skip org option lines (#+title:, #+description:, etc.)
               ((looking-at "^#\\+")
                (forward-line 1))
               ;; Skip org headings
               ((looking-at "^\\*+\\s-")
                (forward-line 1))
               ;; Skip empty lines
               ((looking-at "^\\s-*$")
                (forward-line 1))
               ;; Process regular content paragraphs
               (t
                (let ((para-start (point)))
                  ;; Find end of current paragraph (stop at empty line, heading, or option)
                  (while (and (not (eobp))
                              (not (looking-at "^\\s-*$"))     ; Not empty line
                              (not (looking-at "^\\*+\\s-"))   ; Not heading
                              (not (looking-at "^#\\+")))      ; Not option line
                    (forward-line 1))
                  (let ((para-end (point)))
                    (when (> para-end para-start)
                      ;; Fill this paragraph
                      (fill-region para-start para-end)))))))
            (buffer-string))))
    ;; Replace the content
    (save-excursion
      (delete-region beg end-pos)
      (goto-char beg)
      (insert processed-text))))

(provide 'setup-org-mode)
