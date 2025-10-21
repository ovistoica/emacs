;;; Code:

(use-package significant-other
  :straight '(significant-other :type git :host github :repo "ovistoica/significant-other.el")
  :bind ("s-j" . significant-other-jump))

(provide 'setup-significant-other)

