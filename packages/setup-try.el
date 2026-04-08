;;; setup-try.el --- Emacs wrapper for the `try` ephemeral workspace manager -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides interactive Emacs commands that mirror the behaviour of the `try'
;; CLI tool (https://github.com/tobi/try).
;;
;; Commands:
;;   `try'        — fuzzy-select an existing try directory and open it in Dired.
;;   `try-clone'  — clone a git repo into the tries directory (like `try clone').
;;
;; The tries directory is read from the TRY_PATH environment variable, falling
;; back to ~/src/tries (the same default as the CLI tool).
;;
;; Keybindings:
;;   C-c t t   try
;;   C-c t c   try-clone

;;; Code:

(declare-function dired "dired" (dirname &optional switches))

;; ---------------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------------

(defgroup try nil
  "Emacs wrapper for the `try' ephemeral workspace manager."
  :group 'tools
  :prefix "try-")

(defcustom try-path
  (expand-file-name (or (getenv "TRY_PATH") "~/Work/tries"))
  "Directory that contains all try workspaces.
Mirrors the TRY_PATH environment variable used by the `try' CLI tool."
  :type 'directory
  :group 'try)

(defun try--ensure-dir ()
  "Create `try-path' if it does not exist."
  (unless (file-directory-p try-path)
    (make-directory try-path t)))

(defun try--relative-time (mtime)
  "Return a human-readable relative time string for MTIME.
MTIME should be a Lisp time value (as from `file-attribute-modification-time')."
  (let* ((now (float-time))
         (seconds (- now (float-time mtime)))
         (minutes (/ seconds 60))
         (hours   (/ minutes 60))
         (days    (/ hours 24))
         (weeks   (/ days 7)))
    (cond
     ((< seconds 60)   "just now")
     ((< minutes 60)   (format "%dm ago" (truncate minutes)))
     ((< hours 24)     (format "%dh ago" (truncate hours)))
     ((< days 7)       (format "%dd ago" (truncate days)))
     (t                (format "%dw ago" (truncate weeks))))))

(defun try--list-entries ()
  "Return an alist of (DISPLAY-STRING . FULL-PATH) for every try directory.
Entries are sorted newest-first by modification time.  The display string
mirrors try's TUI: the date prefix is dimmed and the relative mtime is shown
on the right, separated by spaces."
  (try--ensure-dir)
  (let* ((entries
          (delq nil
                (mapcar
                 (lambda (name)
                   (let ((path (expand-file-name name try-path)))
                     (when (file-directory-p path)
                       (let* ((attrs (file-attributes path))
                              (mtime (file-attribute-modification-time attrs)))
                         (list name path mtime)))))
                 (directory-files try-path nil "^[^.]"))))
         ;; Sort newest-first
         (sorted (sort entries
                       (lambda (a b)
                         (time-less-p (nth 2 b) (nth 2 a))))))
    (mapcar
     (lambda (entry)
       (pcase-let ((`(,name ,path ,mtime) entry))
         ;; Style: dim the YYYY-MM-DD prefix, keep the rest of the name plain
         (let* ((display
                 (if (string-match
                      (rx bol (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
                          "-" (group (+ anything)))
                      name)
                     (concat
                      (propertize (match-string 1 name) 'face 'shadow)
                      (propertize "-" 'face 'shadow)
                      (match-string 2 name))
                   name))
                (rel-time (try--relative-time mtime))
                (candidate (concat display
                                   (propertize (concat "  " rel-time)
                                               'face 'shadow))))
           (cons candidate path))))
     sorted)))

(defun try--parse-git-uri (uri)
  "Extract (USER REPO) from a git URI string.
Handles https://github.com/user/repo, git@github.com:user/repo,
and equivalent formats on other hosts.  Returns nil if unparseable."
  (let ((uri (replace-regexp-in-string "\\.git$" "" uri)))
    (cond
     ;; https://github.com/user/repo  or  https://host/user/repo
     ((string-match
       (rx "://" (+ (not "/")) "/" (group (+ (not "/"))) "/" (group (+ (not "/"))))
       uri)
      (list (match-string 1 uri) (match-string 2 uri)))
     ;; git@github.com:user/repo
     ((string-match
       (rx "git@" (+ (not ":")) ":" (group (+ (not "/"))) "/" (group (+ anything)))
       uri)
      (list (match-string 1 uri) (match-string 2 uri)))
     (t nil))))

(defun try--generate-clone-dir-name (git-uri &optional custom-name)
  "Compute the dated directory name for cloning GIT-URI.
If CUSTOM-NAME is non-nil and non-empty it is used verbatim.
Otherwise the name is derived from the URI as YYYY-MM-DD-USER-REPO."
  (if (and custom-name (not (string-empty-p custom-name)))
      custom-name
    (let ((parsed (try--parse-git-uri git-uri)))
      (unless parsed
        (user-error "Unable to parse git URI: %s" git-uri))
      (format "%s-%s-%s"
              (format-time-string "%Y-%m-%d")
              (car parsed)
              (cadr parsed)))))

;; ---------------------------------------------------------------------------
;; Interactive commands
;; ---------------------------------------------------------------------------

;;;###autoload
(defun try (&optional initial-input)
  "Select a try workspace and open it in Dired.
Presents a completing-read list of all directories in `try-path',
sorted newest-first and annotated with relative modification times —
mirroring the `try' TUI selector.

With a prefix argument, prompt for INITIAL-INPUT to pre-filter the list."
  (interactive
   (list (when current-prefix-arg
           (read-string "Filter tries: "))))
  (try--ensure-dir)
  (let* ((entries (try--list-entries))
         (choices (mapcar #'car entries))
         ;; Wrap in a completion table that preserves recency order.
         ;; Without this, UIs like Vertico re-sort candidates alphabetically.
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function   . identity))
                    (complete-with-action action choices string pred))))
         (selection (completing-read "Try: " table nil nil initial-input)))
    (when (and selection (not (string-empty-p selection)))
      (if-let ((path (cdr (assoc selection entries))))
          (dired path)
        ;; The user typed something that did not match — create a new try dir
        (let* ((date-prefix (format-time-string "%Y-%m-%d"))
               (name (format "%s-%s" date-prefix selection))
               (new-path (expand-file-name name try-path)))
          (make-directory new-path t)
          (message "Created new try: %s" new-path)
          (dired new-path))))))

;;;###autoload
(defun try-clone (git-uri &optional custom-name)
  "Clone GIT-URI into the tries directory, then open it in Dired.
Prompts for GIT-URI and an optional CUSTOM-NAME override.
Mirrors `try clone <url> [name]'."
  (interactive
   (let* ((uri (read-string "Git URI to clone: "))
          (name (read-string
                 (format "Directory name (leave blank for auto: %s): "
                         (condition-case _
                             (apply #'format "%s-%s-%s"
                                    (format-time-string "%Y-%m-%d")
                                    (try--parse-git-uri uri))
                           (error "…")))
                 nil nil "")))
     (list uri name)))
  (try--ensure-dir)
  (let* ((dir-name (try--generate-clone-dir-name git-uri custom-name))
         (dest     (expand-file-name dir-name try-path)))
    (when (file-directory-p dest)
      (user-error "Destination already exists: %s" dest))
    (message "Cloning %s into %s …" git-uri dest)
    ;; Remove the pre-created dir — git clone needs to create it itself
    (delete-directory dest t)
    (compilation-start
     (format "git clone %s %s"
             (shell-quote-argument git-uri)
             (shell-quote-argument dest))
     'compilation-mode
     (lambda (_) (format "*try clone: %s*" dir-name)))
    ;; Open dired once clone finishes via compilation-finish-functions hook
    (letrec ((sentinel-hook
              (lambda (_buf _msg)
                (remove-hook 'compilation-finish-functions sentinel-hook)
                (when (file-directory-p dest)
                  (dired dest)))))
      (add-hook 'compilation-finish-functions sentinel-hook))))

;; ---------------------------------------------------------------------------
;; Keymap
;; ---------------------------------------------------------------------------

(defvar try-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'try)
    (define-key map (kbd "c") #'try-clone)
    map)
  "Keymap for try commands, bound under \\`C-c t'.")

(global-set-key (kbd "C-c t") try-map)

(provide 'setup-try)
;;; setup-try.el ends here
