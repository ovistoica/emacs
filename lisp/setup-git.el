;;; setup-git.el --- Config related to git -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . magit-git-commit-insert-branch))
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :preface
  (defun magit-extract-branch-tag (branch-name)
    "Extract branch tag from BRANCH-NAME."
    (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
      (when (string-match-p ticket-pattern branch-name)
        (upcase (replace-regexp-in-string ticket-pattern "\\1: \n" branch-name)))))
  (defun magit-git-commit-insert-branch ()
    "Insert the branch tag in the commit buffer if feasible."
    (when-let ((tag (magit-extract-branch-tag (magit-get-current-branch))))
      (insert tag)
      (forward-char -1))))



(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

;; ** GIT-GUTTER
(use-package git-gutter
  :ensure git-gutter
  :diminish ""
  :functions
  global-git-gutter-mode
  :config
  (global-git-gutter-mode t))

(provide 'setup-git)
;;; setup-git.el ends here
