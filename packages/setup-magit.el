(use-package magit
  :defer t
  :custom
  (magit-section-initial-visibility-alist '((untracked . show)
                                            (unstaged . show)
                                            (unpushed . show)
                                            (unpulled . show)
                                            (stashes . show)))
  (magit-diff-regine-hunk t)
  (magit-push-always-verify nil)
  (magit-no-confirm '(stage-all-changes
                      unstage-all-changes))

  :bind ((:map magit-status-mode-map
               ("q" . magit-quit)))
  :config
  (wrap-fullscreen magit-status)
  (wrap-fullscreen magit-init)

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


(provide 'setup-magit)
