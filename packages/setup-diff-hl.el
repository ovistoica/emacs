;; diff-hl-mode
;;
;; Highlights uncomitted changes on the left side of the widnow (area also known
;; as the "gutter"), allows you to jump between and revert them selectively.

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(provide 'setup-diff-hl)
;;; setup-diff-hl.el ends here
