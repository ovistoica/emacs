;; Linux-specific configuration

;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-shell-name "/bin/bash") ; Use bash on Linux
  (setq exec-path-from-shell-variables
        '(
          "ANTHROPIC_API_KEY"
          "OPENAI_API_KEY"
          "MANPATH"
          "PATH"
          "SSH_AGENT_PID"
          "SSH_AUTH_SOCK"
          ))
  (exec-path-from-shell-initialize))

(provide 'linux-setup)
