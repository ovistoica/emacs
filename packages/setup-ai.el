;;; setup-ai.el --- AI tooling configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-related packages.

;;; Code:

(defun my/agent-shell-toggle ()
  "Toggle agent shell display, falling back to any shell if none for project."
  (interactive)
  (if-let ((shell-buffer (or (and (derived-mode-p 'agent-shell-mode)
                                  (current-buffer))
                             (seq-first (agent-shell-project-buffers))
                             (seq-first (agent-shell-buffers)))))
      (if-let ((window (get-buffer-window shell-buffer)))
          (if (> (count-windows) 1)
              (delete-window window)
            (switch-to-prev-buffer))
        (agent-shell--display-buffer shell-buffer))
    (call-interactively #'agent-shell)))

(use-package agent-shell
  :ensure t
  :init
  (require 'diff)
  :bind (("C-c C-;" . agent-shell)
         ("C-c ;"   . my/agent-shell-toggle)
         ("C-c '"   . agent-shell-menu))
  :custom
  ;; Window configuration
  (agent-shell-display-action
   '((display-buffer-in-side-window)
     (side . right)
     (window-width . 90)))
  ;; Show icons for different agent configs
  (agent-shell-show-config-icons t)
  ;; Expand tool use blocks by default
  (agent-shell-tool-use-expand-by-default nil)
  ;; Context sources for prompts
  (agent-shell-context-sources '(files region error line))
  :config
  (transient-define-prefix agent-shell-menu ()
    "Agent Shell commands."
    ["Shell"
     [("a" "Open shell" agent-shell)
      ("t" "Toggle shell" agent-shell-toggle)
      ("n" "New shell" agent-shell-new-shell)
      ("c" "Compose prompt" agent-shell-prompt-compose)
      ("o" "Other buffer" agent-shell-other-buffer)]
     [("i" "Interrupt" agent-shell-interrupt)
      ("r" "Resume pending" agent-shell-resume-pending-requests)
      ("R" "Remove pending" agent-shell-remove-pending-request)]]
    ["Session"
     [("m" "Set model" agent-shell-set-session-model)
      ("M" "Set mode" agent-shell-set-session-mode)
      ("C" "Cycle mode" agent-shell-cycle-session-mode)]]
    ["Send"
     [("f" "Send file" agent-shell-send-file)
      ("F" "Send other file" agent-shell-send-other-file)
      ("s" "Send screenshot" agent-shell-send-screenshot)
      ("!" "Insert shell output" agent-shell-insert-shell-command-output)]]
    ["Logs & Debug"
     [("T" "Open transcript" agent-shell-open-transcript)
      ("v" "View traffic" agent-shell-view-traffic)
      ("l" "View ACP logs" agent-shell-view-acp-logs)
      ("L" "Toggle logging" agent-shell-toggle-logging)
      ("V" "Version" agent-shell-version)]]))

(provide 'setup-ai)
;;; setup-ai.el ends here
