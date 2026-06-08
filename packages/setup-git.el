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
  (magit-diff-regine-hunk t)
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
  :vc (:url "https://github.com/artawower/blamer.el")
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

(use-package magit-difftastic
  :vc (:url "https://github.com/rschmukler/magit-difftastic")
  :defer t
  :after magit
  :commands (magit-difftastic-mode magit-difftastic-toggle-file-rendering)
  :defines (magit-difftastic-mode
            magit-difftastic-hunk-section-map
            ansi-color-normal-colors-vector
            ansi-color-bright-colors-vector)
  :functions (magit-difftastic-clear-cache
              magit-refresh
              color-name-to-rgb
              color-rgb-to-hex
              magit-section-highlight-range
              magit-difftastic--parse-chunk-lines)
  :preface
  (defun my/magit-difftastic-toggle ()
    "Toggle difftastic rendering in Magit and refresh the current buffer.
`magit-difftastic-mode' is a global mode that renders diffs with
difftastic only while enabled.  Toggle it on demand and refresh so the
current Magit buffer reflects the change immediately."
    (interactive)
    (magit-difftastic-mode 'toggle)
    (when (derived-mode-p 'magit-mode)
      (magit-refresh))
    (message "Difftastic rendering %s"
             (if magit-difftastic-mode "enabled" "disabled")))

  ;; Calmer, Magit-like diff colors for difftastic.
  ;;
  ;; difft colors the whole changed line's *foreground* (bright red/green), and
  ;; the `difftastic' package maps those onto `magit-diff-removed'/`magit-diff-added'
  ;; and adds the matching background.  Under saturated themes the loud foreground
  ;; looks busy.  We keep Magit's background band but desaturate the foreground by
  ;; blending it toward the default text color, so changed lines read as a quiet
  ;; tint instead of solid red/green.  (difft can't show major-mode syntax colors
  ;; on changed lines -- it owns their foreground -- so a calm blended foreground
  ;; is the closest practical match to Magit's look; context lines keep syntax.)
  ;;
  ;; The blend must stay > 0 so added/removed keep *distinct* foregrounds: the
  ;; difftastic background is added by matching difft's foreground against these
  ;; faces, and identical foregrounds would make the matcher pick the wrong band.
  (defcustom my/magit-difftastic-fg-blend 0.55
    "Fraction of the diff color kept in changed-line text.
0.0 = default text color (loses add/remove distinction), 1.0 = full
difft red/green.  Lower is calmer; must stay above 0."
    :type 'number
    :group 'magit-difftastic)

  (defface my/magit-difftastic-added nil
    "Calmer added face for difftastic output (Magit background, muted foreground).")

  (defface my/magit-difftastic-removed nil
    "Calmer removed face for difftastic output (Magit background, muted foreground).")

  (defun my/color-blend (a b alpha)
    "Blend hex colors A over B by ALPHA (0..1); return a hex string or nil."
    (when (and (stringp a) (stringp b))
      (let ((c1 (color-name-to-rgb a))
            (c2 (color-name-to-rgb b)))
        (when (and c1 c2)
          (color-rgb-to-hex
           (+ (* alpha (nth 0 c1)) (* (- 1 alpha) (nth 0 c2)))
           (+ (* alpha (nth 1 c1)) (* (- 1 alpha) (nth 1 c2)))
           (+ (* alpha (nth 2 c1)) (* (- 1 alpha) (nth 2 c2)))
           2)))))

  (defun my/magit-difftastic-sync-faces (&rest _)
    "Sync difftastic diff faces to the current theme's `magit-diff-*' faces.
Keeps each background but blends the foreground toward the default text
color (see `my/magit-difftastic-fg-blend').  Added to `enable-theme-functions'
so it tracks Omarchy theme switches.  Clears difftastic's render cache so a
subsequent Magit refresh re-renders with the new colors."
    (when (facep 'magit-diff-added)
      (let ((deffg (face-attribute 'default :foreground nil t)))
        (dolist (spec '((my/magit-difftastic-added   . magit-diff-added)
                        (my/magit-difftastic-removed . magit-diff-removed)))
          (let* ((src  (cdr spec))
                 (fg   (face-attribute src :foreground nil t))
                 (bg   (face-attribute src :background nil t))
                 (calm (or (my/color-blend fg deffg my/magit-difftastic-fg-blend) fg)))
            (set-face-attribute (car spec) nil
                                :foreground (if (stringp calm) calm 'unspecified)
                                :background (if (stringp bg) bg 'unspecified)
                                :extend t))))
      (when (fboundp 'magit-difftastic-clear-cache)
        (magit-difftastic-clear-cache))))

  (defun my/magit-difftastic-section-highlight (orig section &rest args)
    "Highlight only the heading of difftastic chunks, leaving the body readable.
Magit's default highlighter (`magit-section-highlight') paints the whole
current section with the flat `magit-section-highlight' face.  Normal hunks
avoid this because they are \"painted\" and use the diff `*-highlight' faces,
but difftastic's custom `magit-difftastic-hunk' sections fall back to the flat
wash, which obscures the rendered code when point is on the chunk.  For those
sections, highlight just the heading (so the current chunk is still marked)
and skip the body wash; all other sections use ORIG with ARGS unchanged."
    (if (and (eq (slot-value section 'type) 'magit-difftastic-hunk)
             (slot-value section 'heading-highlight-face))
        (magit-section-highlight-range
         (slot-value section 'start)
         (or (slot-value section 'content) (slot-value section 'end))
         (slot-value section 'heading-highlight-face))
      (apply orig section args)))

  (defun my/magit-difftastic--visit-line-at-point (section)
    "Return the worktree line for the difftastic display row at point in SECTION.
Reads the gutter numbers of the row under point (new/rhs side preferred, old/lhs
for deletions) from difftastic's per-row parse, so visiting lands on the exact
line at point.  Returns nil when point is not on a numbered row (the caller then
falls back to the chunk's first change)."
    (let ((pt (point)))
      (seq-some
       (lambda (row)
         (pcase-let ((`((,bol ,eol) ,left ,right) row))
           (when (and (integerp bol) (integerp eol) (<= bol pt) (<= pt eol))
             (or (car right) (car left)))))
       (magit-difftastic--parse-chunk-lines section))))

  (defun my/magit-difftastic-chunk-visit-line-at-point (orig section)
    "Visit the line at point in a difftastic chunk, not just its first change.
Around-advice for `magit-difftastic--chunk-visit-line': prefer the file line of
the display row under point (so `C-j' etc. land where point is, like vanilla
Magit); fall back to ORIG (the chunk's first change) when point is not on a
numbered row."
    (or (ignore-errors (my/magit-difftastic--visit-line-at-point section))
        (funcall orig section)))
  :bind (:map magit-mode-map
              ("C-c d" . my/magit-difftastic-toggle))
  :init
  ;; Add an on-demand entry to the magit diff transient (the `d' menu),
  ;; anchored after the "t" (Show stash) suffix.  `:after magit' guarantees
  ;; `magit-diff' exists when this runs.  Anchoring to a suffix key (rather
  ;; than group coordinates like `(-1 -1)') avoids the "suffixes and groups
  ;; cannot be siblings" error.
  (transient-append-suffix 'magit-diff "t"
    '("M-d" "Difftastic rendering (toggle)" my/magit-difftastic-toggle))
  :config
  ;; Restore Magit's section-level keys on difftastic chunks.
  ;;
  ;; Magit binds most per-section commands (C-j/C-<return> to visit, the
  ;; `[remap magit-stage-files]'-style staging entries, etc.) in the section's
  ;; text-property keymap -- `magit-hunk-section-map' for normal hunks -- NOT in
  ;; `magit-mode-map'.  difftastic chunks use their own near-empty
  ;; `magit-difftastic-hunk-section-map' with no parent, so those keys fall
  ;; through to the global map (e.g. C-j -> `electric-newline-and-maybe-indent').
  ;; Parent it to `magit-hunk-section-map' so all hunk keys are inherited; they
  ;; route through magit-difftastic's command advice, which handles difftastic
  ;; sections (visit/stage/unstage/discard) correctly.
  (set-keymap-parent magit-difftastic-hunk-section-map magit-hunk-section-map)

  ;; Route difft's added/removed colors through our calmer faces (slots 1/2),
  ;; keeping difftastic's defaults for the rest.  `difftastic' is loaded as a
  ;; dependency, so its color vectors and `ansi-color' are available here.
  (require 'difftastic)
  (require 'ansi-color)
  (dolist (v '(difftastic-normal-colors-vector difftastic-bright-colors-vector))
    (let ((base (if (eq v 'difftastic-normal-colors-vector)
                    ansi-color-normal-colors-vector
                  ansi-color-bright-colors-vector)))
      (set v (vector (aref base 0)
                     'my/magit-difftastic-removed
                     'my/magit-difftastic-added
                     'magit-diff-file-heading
                     'font-lock-comment-face
                     'font-lock-string-face
                     'font-lock-warning-face
                     (aref base 7)))))
  (my/magit-difftastic-sync-faces)
  (add-hook 'enable-theme-functions #'my/magit-difftastic-sync-faces)

  ;; Keep difftastic chunks readable when point is on them: highlight only the
  ;; heading instead of washing the whole body with `magit-section-highlight'.
  (advice-add 'magit-section-highlight :around
              #'my/magit-difftastic-section-highlight)

  ;; Make visiting (RET, C-j, ...) land on the line at point inside a difftastic
  ;; chunk, rather than the chunk's first change.
  (advice-add 'magit-difftastic--chunk-visit-line :around
              #'my/magit-difftastic-chunk-visit-line-at-point))

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
