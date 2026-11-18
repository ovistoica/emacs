;;; setup-git --- Settings and packages related to git management -*- lexical-binding: t -*-

;;; Commentary:
;;; I mostly use magit for usual git but this file contains some other quality of life packages.

;;; Code:

(declare-function magit-git-lines "magit-git")

(use-package magit
  :defer t
  :custom
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (unstaged . show)
                                            (unpushed . show)
                                            (unpulled . show)
                                            (stashes . hide)))
  (magit-diff-refine-hunk t)
  (magit-no-confirm '(stage-all-changes
                      unstage-all-changes))

  :bind ((:map magit-status-mode-map
               ("q" . magit-quit)
               ("C-c p r" . my/magit-create-bitbucket-pr)))
  :config
  (wrap-fullscreen magit-status)
  (wrap-fullscreen magit-init)

  ;; Show worktrees section in magit status buffer
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-worktrees
                          nil t)

  ;; Faster status refresh: intentionally drop the tags header and the
  ;; Head/Merge/Push headers, which each cost extra git calls.
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)

  ;; move cursor into position when entering commit message
  (add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)

  ;; TAB on the empty subject line offers previous "prefix: " subjects;
  ;; elsewhere the filter yields nil so TAB keeps its usual binding.
  ;; `magit' requires `git-commit', so the keymap exists here.  <tab> is
  ;; bound too, since GUI frames only translate <tab> to TAB when no
  ;; active keymap binds <tab> (e.g. snippet/completion minor modes).
  (dolist (key '("TAB" "<tab>"))
    (keymap-set git-commit-mode-map key
                '(menu-item "" my/git-choose-subject
                            :filter (lambda (cmd)
                                      (when (my/git-subject-position?) cmd))))))

(use-package git-timemachine
  :defer t
  :bind (("C-x v t" . git-timemachine)))

(use-package browse-at-remote
  :defer t
  :custom
  (browse-at-remote-prefer-symbolic nil)
  :bind (("C-x v w" . browse-at-remote-kill)))

(use-package blamer
  :bind (("s-i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 10)
  (blamer-author-formatter "  ✎ %s ")
  (blamer-datetime-formatter "[%s]")
  (blamer-commit-formatter " ● %s")
  (blamer-type 'visual)
  (blamer-view 'overlay-right)
  (blamer-max-commit-message-length 70)
  :commands (blamer-mode))

(defun my/face-spec-has-background-p (spec)
  "Return non-nil when face SPEC has a background.
SPEC may be a face symbol, a `face-attribute' plist, or a list of either."
  (cond ((null spec) nil)
        ((facep spec)
         (let ((bg (face-attribute spec :background nil t)))
           (and bg (not (eq bg 'unspecified)))))
        ((and (consp spec) (keywordp (car spec)))
         (and (plist-get spec :background) t))
        ((consp spec)
         (seq-some #'my/face-spec-has-background-p spec))
        (t nil)))

(use-package magit-difftastic
  :vc (:url "https://github.com/rschmukler/magit-difftastic")
  :after magit
  :defines (magit-difftastic-mode
            magit-difftastic-lazy
            ansi-color-normal-colors-vector
            ansi-color-bright-colors-vector
            difftastic-normal-colors-vector
            difftastic-bright-colors-vector
            difftastic--ansi-color-add-background-cache)
  :functions (magit-refresh magit-section-highlight-range
              magit-difftastic-clear-cache)
  :preface
  (defun my/magit-difftastic--next-face-change (pos limit)
    "Return the next position after POS where face properties change.
Consider both `face' and `font-lock-face'; never return a position
past LIMIT."
    (min (or (next-single-property-change pos 'face nil limit) limit)
         (or (next-single-property-change pos 'font-lock-face nil limit) limit)))

  (defun my/magit-difftastic-section-highlight (orig section &rest args)
    "Highlight difftastic chunk SECTION without washing out diff backgrounds.
The chunk heading is highlighted as usual; the body gets the
`magit-section-highlight' tint only where the rendered diff does not
already set its own background, so added/removed line colors survive
section selection.  For other sections, call ORIG with SECTION and ARGS."
    (if (eq (oref section type) 'magit-difftastic-hunk)
        (let ((start (oref section start))
              (content (oref section content))
              (end (oref section end)))
          (magit-section-highlight-range
           start (or content end) (oref section heading-highlight-face))
          (when content
            (let ((pos content))
              (while (< pos end)
                (let ((next (my/magit-difftastic--next-face-change pos end)))
                  (unless (or (my/face-spec-has-background-p
                               (get-text-property pos 'face))
                              (my/face-spec-has-background-p
                               (get-text-property pos 'font-lock-face)))
                    (magit-section-highlight-range pos next))
                  (setq pos next))))))
      (apply orig section args)))

  (defun my/magit-difftastic-eager-refresh (fn &rest args)
    "Call FN with ARGS, rendering difftastic synchronously during its refresh."
    (let ((magit-difftastic-lazy nil))
      (apply fn args)))

  (defun my/magit-difftastic-reset-caches (&rest _)
    "Drop every cached difftastic render so the next refresh uses fresh colors.
Rendered chunks carry literal colour strings, so after a theme change both
`magit-difftastic''s render cache and difftastic's per-buffer face memo
would keep painting the previous theme's colours.  For `enable-theme-functions'."
    (when (fboundp 'magit-difftastic-clear-cache)
      (magit-difftastic-clear-cache))
    (dolist (buf (buffer-list))
      (when (buffer-local-value 'difftastic--ansi-color-add-background-cache buf)
        (with-current-buffer buf
          (setq difftastic--ansi-color-add-background-cache nil)))))

  :custom
  ;; Stock diffs by default (fast on big diffs); C-c C-d opts a file into
  ;; difftastic, C-u C-c C-d flips the whole buffer.
  (magit-difftastic-default-rendering 'stock)
  ;; Render difft output asynchronously: files show "Rendering difftastic…"
  ;; and fill in as each difft subprocess finishes; expanding prioritizes.
  (magit-difftastic-lazy t)
  :config
  ;; Lazy rendering swaps the staged file for a "Rendering difftastic…"
  ;; placeholder until difft finishes, which flickers after every hunk
  ;; stage.  Render synchronously for the refresh these commands trigger;
  ;; the diff is small and unchanged files come from the render cache.
  ;; Depth -100 keeps this outside `magit-difftastic-mode''s own advice, which
  ;; stages a chunk and refreshes without calling the advised command.
  (dolist (cmd '(magit-stage magit-unstage magit-discard))
    (advice-add cmd :around #'my/magit-difftastic-eager-refresh '((depth . -100))))
  ;; The default `magit-section-highlight' lays a flat overlay over the chunk
  ;; body, painting over the added/removed line colors.
  (advice-add 'magit-section-highlight :around
              #'my/magit-difftastic-section-highlight)
  (require 'difftastic)
  (require 'ansi-color)
  ;; Setup theme aware difftastic colors for diffs
  (dolist (v '(difftastic-normal-colors-vector difftastic-bright-colors-vector))
    (let ((base (if (eq v 'difftastic-normal-colors-vector)
                    ansi-color-normal-colors-vector
                  ansi-color-bright-colors-vector)))
      (set v (vector (aref base 0)
                     'magit-diff-removed-highlight
                     'magit-diff-added-highlight
                     'magit-diff-file-heading
                     'font-lock-comment-face
                     'font-lock-string-face
                     'font-lock-warning-face
                     (aref base 7)))))
  ;; Cached renders bake in hex colours; invalidate them on theme switches.
  (add-hook 'enable-theme-functions #'my/magit-difftastic-reset-caches)
  (magit-difftastic-mode 1))

(defun kill-magit-buffers ()
  (let ((current (current-buffer)))
    (dolist (buf (magit-mode-get-buffers))
      (unless (eq buf current)
        (kill-buffer buf)))))

(defun magit-quit ()
  "Like magit-mode-bury-buffer, but also restores the window-configuration
stored by magit-status fullscreen."
  (interactive)
  (let ((prev my/previous-window-configuration))
    (kill-magit-buffers)
    (funcall magit-bury-buffer-function 'kill-buffer)
    (when prev (register-val-jump-to prev nil))))

(defun is-commit-message-buffer? ()
  (when (buffer-file-name)
    (equal (buffer-file-name-body) "COMMIT_EDITMSG")))

(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (while (looking-at "#")
      (forward-line))
    (forward-line)))

(defun my/git-subject-commit-range-safe? (value)
  "Return non-nil if VALUE is a safe `my/git-subject-commit-range'.
Only nil or a string not starting with \"-\" is accepted: the value is
passed to `git log' as an argument, so a leading dash would turn it into
an option such as --output=FILE."
  (or (null value)
      (and (stringp value)
           (not (string-prefix-p "-" value)))))

(defun my/git-subject-info-safe? (value)
  "Return non-nil if VALUE is a list of (SUBJECT . TEXT) string pairs."
  (and (proper-list-p value)
       (seq-every-p (lambda (entry)
                      (and (consp entry)
                           (stringp (car entry))
                           (stringp (cdr entry))))
                    value)))

(defcustom my/git-subject-commit-range nil
  "Git revision range to scan for previous commit subjects.
For example \"abc123^..HEAD\".  nil means the last 9999 commits."
  :type '(choice (const :tag "Last 9999 commits" nil) string)
  :safe #'my/git-subject-commit-range-safe?
  :group 'git-commit)

(defcustom my/git-subject-info nil
  "Alist of (SUBJECT . TEXT) shown when choosing a commit subject.
When SUBJECT is chosen with `my/git-choose-subject', TEXT is inserted
on the lines below it."
  :type '(alist :key-type string :value-type string)
  :safe #'my/git-subject-info-safe?
  :group 'git-commit)

(defun my/git-subject-suggestions ()
  "Return distinct \"prefix: \" subjects of previous commits, most recent first.
Commits are taken from `my/git-subject-commit-range'."
  (thread-last
    (magit-git-lines "log" (or my/git-subject-commit-range "-n9999") "--format=%s")
    (seq-keep (lambda (subject)
                (when (string-match "\\`\\([^:]+\\): " subject)
                  (concat (match-string 1 subject) ": "))))
    (delete-dups)))

(defun my/git-subject-position? ()
  "Return non-nil when point is at the start of an empty first line."
  (and (bobp) (looking-at-p "$")))

(defun my/git-choose-subject ()
  "Insert a commit subject prefix chosen from previous commits.
Suggestions keep recency order.  If the chosen subject has an entry in
`my/git-subject-info', its text is inserted below, leaving point after
the subject."
  (interactive)
  (let* ((suggestions (my/git-subject-suggestions))
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata (display-sort-function . identity)
                                      (cycle-sort-function . identity))
                         (complete-with-action action suggestions string pred))))
         (subject (completing-read "Subject: " collection nil nil)))
    (insert subject)
    (when-let* ((entry (assoc subject my/git-subject-info)))
      (save-excursion
        (newline)
        (insert (cdr entry))
        (newline)))))

(defun my/magit-create-bitbucket-pr ()
  "Generate and open Bitbucket PR URL for the current branch."
  (interactive)
  (let* ((current-branch (magit-get-current-branch))
         (remote-url (magit-get "remote" "origin" "url"))
         (repo-path (when remote-url
                      (cond
                       ;; HTTPS URL: https://bitbucket.org/workspace/repo.git
                       ((string-match "https://bitbucket.org/\\(.+?\\)/\\(.+?\\)\\(?:\\.git\\)?$" remote-url)
                        (format "%s/%s" (match-string 1 remote-url) (match-string 2 remote-url)))
                       ;; SSH URL: git@bitbucket.org:workspace/repo.git
                       ((string-match "git@bitbucket.org:\\(.+?\\)/\\(.+?\\)\\(?:\\.git\\)?$" remote-url)
                        (format "%s/%s" (match-string 1 remote-url) (match-string 2 remote-url)))
                       (t nil))))
         (pr-url (when repo-path
                   (format "https://bitbucket.org/%s/pull-requests/new?source=%s&t=1"
                           repo-path current-branch))))
    (cond
     ((not current-branch)
      (user-error "Not on a git branch"))
     ((not repo-path)
      (user-error "Could not parse Bitbucket repository from remote origin URL: %s" remote-url))
     (t
      (message "Opening PR for branch: %s" current-branch)
      (browse-url pr-url)))))


(provide 'setup-git)
;;; setup-git.el ends here
