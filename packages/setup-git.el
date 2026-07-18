;;; setup-git --- Settings and packages related to git management -*- lexical-binding: t -*-

;;; Commentary:
;;; I mostly use magit for usual git but this file contains some other quality of life packages.

;;; Code:

(use-package magit
  :defer t
  :custom
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (unstaged . show)
                                            (unpushed . show)
                                            (unpulled . show)
                                            (stashes . hide)))
  (magit-diff-refine-hunk t)
  (magit-push-always-verify nil)
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

  ;; move cursor into position when entering commit message
  (add-hook 'git-commit-mode-hook 'my/magit-cursor-fix))

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
  ;; Loads with magit (no :defer): the global mode is enabled in :config;
  ;; C-c C-d toggles per-file rendering.
  :after magit
  :commands (magit-difftastic-mode magit-difftastic-toggle-file-rendering)
  :defines (magit-difftastic-mode
            ansi-color-normal-colors-vector
            ansi-color-bright-colors-vector)
  :functions (magit-refresh magit-section-highlight-range)
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

  :config
  ;; The default `magit-section-highlight' lays a flat overlay over the chunk
  ;; body, painting over the added/removed line colors.
  (advice-add 'magit-section-highlight :around
              #'my/magit-difftastic-section-highlight)
  ;; Upstream's sparse chunk map replaces magit's hunk section map, so
  ;; section-map-only keys (C-j, ...) fall through to the global map.  Parent
  ;; it; upstream advises `magit-diff-visit-*', so parented keys visit the
  ;; correct line.
  (set-keymap-parent magit-difftastic-hunk-section-map magit-hunk-section-map)
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
