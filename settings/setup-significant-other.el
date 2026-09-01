;;; setup-significant-other.el --- Significant other jump configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package significant-other
  :vc (:url "https://github.com/ovistoica/significant-other.el")
  :bind ("s-j" . significant-other-jump))

(provide 'setup-significant-other)

