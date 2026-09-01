;;; setup-expand-region.el --- Expand region configuration -*- lexical-binding: t; -*-

(use-package expand-region
  :defer t
  :bind ("C-@" . er/expand-region))

(provide 'setup-expand-region)
