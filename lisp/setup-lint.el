;;; setup-lint.el --- Lint provider setup -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(provide 'setup-lint)
;;; setup-lint.el ends here
