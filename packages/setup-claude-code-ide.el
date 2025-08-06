;; claude-code-id.el


(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :custom
  ;; Window configuration
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 90)
  (claude-code-ide-window-height 20)
  (claude-code-ide-use-side-window t)
  (claude-code-ide-focus-on-open t)
  (claude-code-ide-focus-claude-after-ediff t)
  (claude-code-ide-show-claude-window-in-ediff t)

  ;; CLI configuration
  (claude-code-ide-cli-path "claude")
  (claude-code-ide-cli-debug nil)
  (claude-code-ide-cli-extra-flags "")
  (claude-code-ide-system-prompt nil)

  ;; MCP tools
  (claude-code-ide-mcp-allowed-tools 'auto)

  ;; Terminal backend
  (claude-code-ide-terminal-backend 'vterm)
  (claude-code-ide-prevent-reflow-glitch t)
  :config
  (claude-code-ide-emacs-tools-setup)) ; Enable Emacs MCP tools
