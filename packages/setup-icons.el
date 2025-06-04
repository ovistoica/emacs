;;; setup-icons.el --- Icons configuration -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:


(use-package nerd-icons
  :straight '(nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))

(use-package nerd-icons-dired
  :straight '(nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :straight '(nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'setup-icons)
;;; setup-icons.el ends here
