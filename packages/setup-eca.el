;;; setup-eca --- A vendor neutral agent interface -*- lexical-binding: t -*-

;;; Commentary:
;; Eca - Editor Code Assistant

;;; Code:
(require 'transient)


(defun my/eca-chat-mode-hook ()
  "Disable various minor modes in ECA chat buffers for cleaner experience."
  (when (fboundp 'denote-rename-buffer-mode) (denote-rename-buffer-mode -1))
  )

(defun my/eca-send-prompt-from-minibuffer ()
  "Prompt for a message in the minibuffer and send it to the current ECA chat."
  (interactive)
  (let ((prompt (read-string "ECA Prompt: ")))
    (when (and prompt (not (string-empty-p prompt)))
      (eca-chat-send-prompt prompt))))


(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs"
            :branch "master"
            :lisp-dir "."
            :main-file "eca.el")
  :hook (eca-chat-mode . my/eca-chat-mode-hook)
  :bind (("C-c ." . eca-transient-menu)
         ("C-c e" . eca-chat-toggle-window)
         ("C-c i" . eca-chat-add-context-to-user-prompt))
  :ensure t
  :config
  ;; Customize transient menu keybindings
  ;; Replace "p" (repeat prompt) with minibuffer prompt (more common use case)
  (transient-suffix-put 'eca-transient-menu '(0 0 6) :key "P")
  (transient-append-suffix 'eca-transient-menu '(0 0 6)
    '("p" "Send prompt from minibuffer" my/eca-send-prompt-from-minibuffer))

  :custom
  ;;(setq eca-extra-args '("--verbose"))
  (setq eca-chat-auto-add-repomap t)

  )

(provide 'setup-eca)
;;; setup-eca.el ends here
