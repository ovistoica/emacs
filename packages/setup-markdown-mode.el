(use-package markdown
  :ensure nil
  :defer t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :init
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

(provide 'setup-markdown-mode)
