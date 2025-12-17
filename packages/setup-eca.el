;;; setup-eca --- A vendor neutral agent interface -*- lexical-binding: t -*-

;;; Commentary:
;; Eca - Editor Code Assistant

;;; Code:
(use-package eca
  :hook (eca-chat-mode . my/eca-chat-mode-hook)
  ;;:config
  ;;(setq eca-extra-args '("--verbose"))
  ;;(setq eca-chat-auto-add-repomap nil)
  :ensure t
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs"
            :lisp-dir "."
            :main-file "eca.el")

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
;;; setup-eca.el ends here
