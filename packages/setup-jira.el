;;; setup-jira.el --- Jira integration via jira.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures jira.el for TeamOhana's Atlassian Jira instance.
;;
;; The Jira API token is fetched lazily from 1Password on the first call
;; to `jira-issues', so Emacs startup is unaffected even if 1Password is
;; not yet unlocked.
;;
;; Saved queries are defined in `my/jira-saved-queries' and merged into
;; `jira-filters' after the package initialises.  They appear instantly
;; under the `F  Apply named filter' action in the issues list menu.
;;
;; Two ways to use a saved query:
;;   1. `M-x jira-issues', then press `F' and select by name.
;;   2. Call `M-x my/jira-open-saved-query' for a standalone prompt.

;;; Code:

(declare-function jira-issues--reset-pagination "jira-issues")
(declare-function jira-issues--fetch-and-display "jira-issues")

(defvar jira-token)

;;; Token — lazy fetch from 1Password ----------------------------------

(defconst my/jira-1p-token-ref "op://TeamOhana/Jira CLI Token/credential"
  "1Password secret reference for the Jira API token.")

(defun my/jira-fetch-token-from-1password ()
  "Fetch the Jira API token from 1Password and set `jira-token'.
Signals a `user-error' if the `op' CLI is not found or not signed in."
  (unless (executable-find "op")
    (user-error "1Password CLI `op' not found on PATH"))
  (let* ((result (string-trim
                  (shell-command-to-string
                   (concat "op read " (shell-quote-argument my/jira-1p-token-ref)))))
         (failed (or (string-empty-p result)
                     (string-match-p "\\[ERROR\\]" result))))
    (if failed
        (user-error "Could not read Jira token from 1Password (is `op' signed in?)")
      (setq jira-token result)
      (message "jira: token loaded from 1Password"))))

(defun my/jira--ensure-token-then-call (orig-fn &rest args)
  "Fetch the Jira token from 1Password if not yet set, then call ORIG-FN with ARGS.
Self-removes after the first successful fetch so subsequent calls
pay no overhead."
  (when (or (not (boundp 'jira-token))
            (null jira-token)
            (string-empty-p jira-token))
    (my/jira-fetch-token-from-1password))
  ;; Remove self so this overhead never runs again in this session.
  (advice-remove 'jira-issues #'my/jira--ensure-token-then-call)
  (apply orig-fn args))

(use-package jira
  :config
  (setq jira-base-url "https://teamohana.atlassian.net")
  (setq jira-username "ovi@teamohana.com")
  ;; Token is intentionally not set here — fetched lazily on first use.
  (setq jira-token nil)
  (setq jira-token-is-personal-access-token nil)
  (setq jira-api-version 3)

  ;; Register the lazy-fetch advice.
  (advice-add 'jira-issues :around #'my/jira--ensure-token-then-call)

  ;; Show a useful default set of columns in the issues table.
  (setq jira-issues-table-fields
        '(:key :issue-type-name :status-name :assignee-name :summary))

  ;; Default to no type filter — let queries control scope.
  (setq jira-issues-default-type nil)

  ;; Fetch more issues per page (default is 30).
  (setq jira-issues-max-results 100)

  ;; Seed jira-filters with local saved queries immediately so they are
  ;; available before (or instead of) the async API fetch of Atlassian
  ;; "My Filters".  jira-api-get-basic-data will append API filters
  ;; later; duplicates are keyed by name so API filters with the same
  ;; name will naturally shadow these.
  (my/jira-merge-saved-queries))

;;; Saved queries -------------------------------------------------------
;;
;; `jira-filters' is an alist of (NAME . JQL) used by the built-in
;; `F  Apply named filter' action in the issues list.
;;
;; Define your queries here; `my/jira-merge-saved-queries' merges them
;; in without clobbering any entries already fetched from the API.

(defvar my/jira-saved-queries
  '(("My open issues"
     . "assignee = currentUser() AND statusCategory != Done ORDER BY updated DESC")
    ("Current sprint — mine"
     . "assignee = currentUser() AND sprint in openSprints() ORDER BY priority ASC")
    ("In progress"
     . "assignee = currentUser() AND status = \"In Progress\" ORDER BY updated DESC")
    ("Current sprint — unassigned"
     . "assignee is EMPTY AND sprint in openSprints() AND statusCategory != Done ORDER BY priority ASC")
    ("Reported by me — open"
     . "reporter = currentUser() AND statusCategory != Done ORDER BY created DESC")
    ("Completed last 2 weeks"
     . "assignee = currentUser() AND statusCategory = Done AND updated >= -2w ORDER BY updated DESC"))
  "Alist of (NAME . JQL) saved Jira queries.
Merged into `jira-filters' on startup so they appear under the
`F  Apply named filter' action in the Jira issues list.")

(defun my/jira-merge-saved-queries ()
  "Merge `my/jira-saved-queries' into `jira-filters'.
Entries already present in `jira-filters' (fetched from the API)
take precedence; local queries fill in anything missing."
  (dolist (entry my/jira-saved-queries)
    (unless (assoc (car entry) jira-filters)
      (push entry jira-filters))))

;; Re-merge after jira-api-get-basic-data runs (it resets jira-filters
;; with the API response, which would wipe local entries).
(add-hook 'jira-issues-mode-hook #'my/jira-merge-saved-queries)

;;; Quick-open command --------------------------------------------------

(defun my/jira-open-saved-query ()
  "Prompt for a saved query by name and open the issues list with it applied."
  (interactive)
  (let* ((all-queries (append my/jira-saved-queries jira-filters))
         (name (completing-read "Jira saved query: " (mapcar #'car all-queries) nil t))
         (jql  (cdr (assoc name all-queries))))
    (when jql
      (jira-issues)
      (setq jira-issues--current-jql jql)
      (jira-issues--reset-pagination)
      (jira-issues--fetch-and-display nil))))

(provide 'setup-jira)
;;; setup-jira.el ends here
