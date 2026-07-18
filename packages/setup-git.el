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
            ansi-color-normal-colors-vector
            ansi-color-bright-colors-vector)
  :functions (magit-refresh)

  :config
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
                     (aref base 7))))))

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
