;; claude-code-ide.el
;; Using parsnips' fork with ghostel backend support (PR #190 against upstream)
;; Once merged upstream, switch back to: :vc (:url "https://github.com/manzaltu/claude-code-ide.el")

(use-package claude-code-ide
  :vc (:url "https://github.com/parsnips/claude-code-ide.el" :branch "codex/ghostel-backend-support")
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

  ;; Terminal backend — set to 'ghostel to test, 'vterm is the default
  (claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-prevent-reflow-glitch t)
  :config
  (claude-code-ide-emacs-tools-setup)) ; Enable Emacs MCP tools
