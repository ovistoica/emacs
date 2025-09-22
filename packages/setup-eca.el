;; Eca - Editor Code Assistant

;;; Code:
(use-package eca
  :straight  '(:type git :host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))

  :hook (eca-chat-mode . my/eca-chat-mode-hook)
  ;;:config
  ;;(setq eca-extra-args '("--verbose"))
  ;;(setq eca-chat-auto-add-repomap nil)

  )

(defun my/eca-chat-mode-hook ()
  (eldoc-mode -1)
  (envrc-mode -1)
  (flycheck-mode -1)
  (js-pkg-mode -1)
  (visual-line-mode -1)
  (whitespace-cleanup-mode -1)
  (dap-mode -1)
  (denote-rename-buffer-mode -1)
  (dap-ui-controls-mode -1)
  (dap-ui-many-windows-mode -1)
  (dap-ui-mode -1))


(provide 'setup-eca)
