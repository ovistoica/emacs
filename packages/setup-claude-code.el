;; setup-claude-code.el

;; Emacs wrapper of claude code

(use-package claude-code
  :after (eat transient)
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  (claude-code-mode))
