;; setup-claude-code.el

;; Emacs wrapper of claude code

(use-package claude-code
  :after (eat transient)
  :diminish ""
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-S-c" . claude-code-command-map)
  :bind (:map claude-code-command-map
              ("Y" . claude-code-send-shift-tab))
  :config
  (claude-code-mode))


(defun claude-code-send-shift-tab ()
  "Send <shift-tab> to the Claude Code REPL.

  This is useful for saying \"Always allow\" when Claude asks for
  confirmation
  without having to switch to the REPL buffer."
  (interactive)
  (if-let ((claude-code-buffer
            (claude-code--get-or-prompt-for-buffer)))
      (with-current-buffer claude-code-buffer
        (eat-term-send-string eat-terminal "\e[Z")
        (display-buffer claude-code-buffer))
    (claude-code--show-not-running-message)))

(provide 'setup-claude-code)
