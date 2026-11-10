;; work.el  -*- lexical-binding: t; -*-

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docker/build"))

(setq world-clock-list
      '(("America/Chicago" "Houston/Texas")
        ("America/New_York" "New York")
        ("America/Los_Angeles" "Los Angeles")
        ("Europe/Bucharest" "Bucharest")
        ("Europe/Brussels" "Belgium")
        ("Europe/London" "UK")
        ("Europe/Madrid" "Spain")
        ("Europe/Stockholm" "Stockholm")
        ("Asia/Jerusalem" "Israel")
        ("Europe/Athens" "Greece")
        ("Asia/Kolkata" "India/Bangalore")
        ("Etc/UTC" "UTC")))

(setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

;;; JIRA Utilities

(declare-function org-element-type "org-element-ast")
(declare-function org-element-context "org-element")
(declare-function org-link-make-string "ol")

(defvar my/jira-ticket-mappings
  '(("\\(?:CHM\\|OP\\|OH\\|PLAT\\)-[0-9]+" . "https://teamohana.atlassian.net/browse/"))
  "Alist of (REGEX . BASE-URL) for matching JIRA ticket patterns to their base URLs.")

(defun my/jira-ticket-url (ticket-number)
  "Convert TICKET-NUMBER to full JIRA URL based on pattern matching.
For example: OH-20979 -> https://teamohana.atlassian.net/browse/OH-20979"
  (let ((match-found nil)
        (url nil))
    (dolist (mapping my/jira-ticket-mappings)
      (when (and (not match-found)
                 (string-match-p (car mapping) ticket-number))
        (setq url (concat (cdr mapping) ticket-number))
        (setq match-found t)))
    url))

(defun my/org-linkify-jira-ticket-at-point ()
  "Wrap the JIRA ticket number at point in an `org-mode' link.
Converts OH-20979 to [[https://teamohana.atlassian.net/browse/OH-20979][OH-20979]]"
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds
          (let* ((ticket (buffer-substring-no-properties (car bounds) (cdr bounds)))
                 (url (my/jira-ticket-url ticket)))
            (if url
                (progn
                  (delete-region (car bounds) (cdr bounds))
                  (insert (format "[[%s][%s]]" url ticket)))
              (message "No matching JIRA pattern found for: %s" ticket)))
        (message "No ticket found at point")))))

(defun my/org-insert-jira-link (full-url)
  "Insert an `org-mode' link FULL-URL - a JIRA ticket URL.
Takes https://teamohana.atlassian.net/browse/OH-20979
and inserts [[https://teamohana.atlassian.net/browse/OH-20979][OH-20979]]"
  (interactive "sJIRA ticket URL: ")
  (let ((ticket-number nil)
        (base-url nil))
    (dolist (mapping my/jira-ticket-mappings)
      (when (and (not ticket-number)
                 (string-match (concat (regexp-quote (cdr mapping)) "\\(" (car mapping) "\\)") full-url))
        (setq ticket-number (match-string 1 full-url))
        (setq base-url (cdr mapping))))
    (if ticket-number
        (insert (format "[[%s][%s]]" full-url ticket-number))
      (message "Invalid JIRA URL format or no matching pattern"))))

(defun my/jira-ticket-before-point ()
  "Return (BEG END TICKET) for a JIRA ticket ending at point, or nil.
TICKET must fully match a regex in `my/jira-ticket-mappings'."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              ((= (point) (cdr bounds)))
              (ticket (buffer-substring-no-properties (car bounds) (cdr bounds)))
              ((seq-some (lambda (mapping)
                           (string-match-p (concat "\\`" (car mapping) "\\'") ticket))
                         my/jira-ticket-mappings)))
    (list (car bounds) (cdr bounds) ticket)))

(defun my/org-tab-linkify-jira-ticket ()
  "Turn the JIRA ticket before point into an Org link, snippet-style.
Meant for `org-tab-first-hook': typing OH-20979 then TAB yields
[[https://teamohana.atlassian.net/browse/OH-20979][OH-20979]].
Does nothing inside an existing link, so TAB keeps its usual behavior."
  (when-let* ((found (my/jira-ticket-before-point))
              ((not (eq (org-element-type (org-element-context)) 'link))))
    (pcase-let ((`(,beg ,end ,ticket) found))
      (delete-region beg end)
      (insert (org-link-make-string (my/jira-ticket-url ticket) ticket)))
    t))

;; Jira helpers
(define-key org-mode-map (kbd "C-x j l") #'my/org-linkify-jira-ticket-at-point)
(define-key org-mode-map (kbd "C-x j i") #'my/org-insert-jira-link)
(add-hook 'org-tab-first-hook #'my/org-tab-linkify-jira-ticket)


(provide 'work)
;;; work.el ends here
