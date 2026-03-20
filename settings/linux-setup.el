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

;; Fallback: fetch API keys from 1Password if not set by shell environment.
;; This handles the case where Emacs runs as a systemd daemon without a login shell.
(defun my/ensure-1password-api-keys ()
  "Fetch API keys from 1Password if they are not already set.
Uses `op read` which requires 1Password CLI to be available and unlocked."
  (unless (getenv "ANTHROPIC_API_KEY")
    (let ((key (string-trim
                (shell-command-to-string
                 "op read 'op://TeamOhana/Anthropic Teamohana API Key/credential' 2>/dev/null"))))
      (when (and key (not (string-empty-p key)) (not (string-match-p "error" key)))
        (setenv "ANTHROPIC_API_KEY" key)))))

;; Run on an idle timer to ensure 1Password agent is available
(run-with-idle-timer 2 nil #'my/ensure-1password-api-keys)

(provide 'linux-setup)
