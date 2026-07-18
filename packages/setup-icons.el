;;; setup-icons.el --- Icons configuration -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:


(use-package nerd-icons)

(use-package nerd-icons-dired
  :diminish " "
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-marginalia-setup))

(provide 'setup-icons)
;;; setup-icons.el ends here
