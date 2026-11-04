;;; setup-expand-region.el --- Expand region configuration -*- lexical-binding: t; -*-

(use-package expand-region
  :defer t
  :bind (("C-@" . er/expand-region)
         ("C-*" . er/contract-region)))

(provide 'setup-expand-region)
