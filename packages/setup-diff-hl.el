;; diff-hl-mode
;;
;; Highlights uncomitted changes on the left side of the widnow (area also known
;; as the "gutter"), allows you to jump between and revert them selectively.

(use-package diff-hl
  :defer t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'setup-diff-hl)
