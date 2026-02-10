;; work.el

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docker/build"))

;; (prodigy-define-service
;;   :name "ShipClojure Datom Shadow-CLJS Watch"
;;   :tags '(shipclojure-datom frontend)
;;   :command "bb"
;;   :args '("watch:frontend")
;;   :cwd "~/workspace/shipclojure-datom"
;;   :stop-signal 'sigkill
;;   :kill-process-buffer-on-stop t)

;; (prodigy-define-service
;;   :name "ShipCLojure Datom CSS Watch"
;;   :tags '(shipclojure-datom css)
;;   :command "bb"
;;   :args '("watch:css")
;;   :cwd "~/workspace/shipclojure-datom"
;;   :stop-signal 'sigkill
;;   :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Wavekit CSS Watch"
  :tags '(wavekit-ai css)
  :command "bb"
  :args '("watch:css")
  :cwd "~/workspace/wavekit-ai"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Shipclojure Website CSS Watch"
  :tags '(shipclojure-website css)
  :command "bb"
  :args '("watch:css")
  :cwd "~/workspace/shipclojure-website"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

;; (prodigy-define-service
;;   :name "Wavekit Shadow-CLJS Watch"
;;   :tags '(wavekit-ai frontend)
;;   :command "bb"
;;   :args '("watch:frontend")
;;   :cwd "~/workspace/wavekit-ai"
;;   :stop-signal 'sigkill
;;   :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "replicant-daisyui CSS Watch"
  :tags '(replicant-daisyui css)
  :command "npm"
  :args '("run" "css:watch")
  :cwd "~/workspace/replicant-daisyui"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "replicant-daisyui Shadow Watch"
  :tags '(replicant-daisyui shadow-cljs)
  :command "npm"
  :args '("run" "shadow:watch")
  :cwd "~/workspace/replicant-daisyui"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)


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

(defvar my/jira-ticket-mappings
  '(("OH-[0-9]+" . "https://teamohana.atlassian.net/browse/"))
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
  "Wrap the JIRA ticket number at point in an org-mode link.
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
  "Insert an org-mode link from a full JIRA ticket URL.
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

(provide 'work)
;;; work.el ends here
